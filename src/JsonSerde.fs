module Fable.Avro.JsonSerde

open System
open FSharp.Reflection
open Fable.SimpleJson
open Fable.Core

let internal rulesMap (rules:CustomRule list) =
    rules
    |> List.map(fun rule ->
        rule.InstanceType.Name,
        {|  SurrogateTypeInfo = createTypeInfo rule.SurrogateType
            CastToSurrogate = rule.CastToSurrogate
            CastFromSurrogate = rule.CastFromSurrogate|})
    |> Map.ofList

type SerializationOptions = {CustomRules: CustomRule list}

let defaultSerializationOptions = {CustomRules = CustomRule.buidInRules}

let createSerializer'(type': Type) (options:SerializationOptions): obj -> Json =
    let ti = createTypeInfo type'
    let rulesMap = options.CustomRules |> rulesMap

    fun (value:obj) ->
        let rec parse (value:obj) (ti:TypeInfo) =
            match ti with
            | TypeInfo.Unit -> JNull
            | TypeInfo.String -> JString(unbox value)
            | TypeInfo.Bool -> JBool(unbox value)
            | TypeInfo.Byte
            | TypeInfo.Short
            | TypeInfo.UInt16
            | TypeInfo.Int32
            | TypeInfo.UInt32
            | TypeInfo.Long
            | TypeInfo.UInt64
            | TypeInfo.Float32
            | TypeInfo.Float -> unbox value |> float |> JNumber
            | TypeInfo.Enum f ->
                let (_, type') = f()
                Enum.GetName(type',value) |> JString
            | TypeInfo.List f ->
                value |> unbox<obj list> |> (parseArray(f()))
            | TypeInfo.Array f ->
                match f() with
                | TypeInfo.Byte ->
                    let hex =
                        unbox<byte []>(value)
                        |> Array.map (fun b -> b.ToString("X2"))
                        |> Seq.ofArray
                        |> String.concat ""
                    JString ("\\u" + hex)
                | ti -> value |> unbox<obj []> |> List.ofArray |> (parseArray ti)
            | TypeInfo.ResizeArray f -> value |> unbox<ResizeArray<obj>> |> List.ofSeq |> (parseArray(f()))
            | TypeInfo.Set f -> value |> unbox<Set<IComparable>> |> List.ofSeq |> List.map box |> (parseArray(f()))
            | TypeInfo.HashSet f -> value |> unbox<Collections.Generic.HashSet<obj>> |> List.ofSeq |> List.map box |> (parseArray(f()))
            | TypeInfo.Seq f -> value |> unbox<obj seq> |> List.ofSeq |> (parseArray(f()))
            | TypeInfo.Map f ->
                let _, valueTypeInfo = f()
                value
                |> unbox<Map<string, obj>>
                |> Map.map (fun _ value -> parse value valueTypeInfo)
                |> JObject
            | TypeInfo.Dictionary f ->
                let _, valueTypeInfo, _ = f()
                value
                |> unbox<Collections.Generic.Dictionary<string, obj>>
                |> Seq.map(fun pair -> pair.Key, parse pair.Value valueTypeInfo)
                |> Map.ofSeq
                |> JObject
            | TypeInfo.Record f ->
                let fields, _ = f()
                fields
                |> Array.map(fun field ->
                    let fieldValue = FSharpValue.GetRecordField(value, field.PropertyInfo)
                    field.FieldName, parse fieldValue field.FieldType)
                |> Map.ofArray
                |> JObject
            | TypeInfo.Tuple f ->
                f()
                |> Array.mapi (fun idx itemTypeInfo ->
                    let itemValue = FSharpValue.GetTupleField(value, idx)
                    (sprintf "Item%d" (idx+1)), parse itemValue itemTypeInfo)
                |> Map.ofArray
                |> JObject
            | TypeInfo.Union f ->
                let unionCases, unionType = f()
                let (usedCaseInfo, fieldValues) = FSharpValue.GetUnionFields(value, unionType)
                let usedCase = unionCases |> Array.find (fun case -> case.CaseName = usedCaseInfo.Name)
                let caseRecord =
                    usedCaseInfo.GetFields()
                    |> Array.mapi (fun idx pi -> pi.Name, parse fieldValues.[idx] usedCase.CaseTypes.[idx])
                    |> Map.ofArray
                    |> JObject
                [(Schema.nameFromType unionType) + "." + usedCaseInfo.Name, caseRecord]
                |> Map.ofList
                |> JObject
            | TypeInfo.Option f ->
                match unbox<obj option> value with
                | None -> JNull
                | Some v ->
                    let typeInfo = f()
                    [Schema.nameFromTypeInfo true typeInfo, parse v typeInfo] |> Map.ofList |> JObject
            | TypeInfo.Decimal -> (unbox value) |> JNumber
            | TypeInfo.Guid -> (unbox<Guid> value).ToString() |> JString
            | TypeInfo.DateTime -> (unbox<DateTime> value).ToString("O", Globalization.CultureInfo.InvariantCulture) |> JString
            | TypeInfo.DateTimeOffset -> (unbox<DateTimeOffset> value).ToString("O", Globalization.CultureInfo.InvariantCulture) |> JString
            | TypeInfo.TimeSpan -> unbox<int> value |> float |> JNumber
            | TypeInfo.BigInt -> unbox<Numerics.BigInteger> value |> string |> JString
            | TypeInfo.Any f ->
                let type' = f()
                match rulesMap.TryFind type'.Name with
                | Some rule -> parse (rule.CastToSurrogate value) rule.SurrogateTypeInfo
                | _ -> failwithf "Serialization of the type is not supported: %A" type'
            | wrong -> failwithf "Serialization of the type is not supported: %A" wrong
        and parseArray elTypeInfo elements  =
            elements
            |> List.map (fun el -> parse el elTypeInfo)
            |> JArray

        parse value ti

let inline createSerializer<'T> (options:SerializationOptions): 'T -> Result<Json, string> =
    let serializer = createSerializer' typeof<'T> options
    fun (value:'T) ->
        try
            serializer value |> Ok
        with
        | err -> Error err.Message

type DeserializationOptions = {CustomRules: CustomRule list; Annotations: string; EvolutionTolerantMode: bool}

let defaultDeserializationOptions = {CustomRules = CustomRule.buidInRules; Annotations = ""; EvolutionTolerantMode = true}

let createDeserializer' (type': Type) (options:DeserializationOptions): Json -> obj =
    let ti = createTypeInfo type'
    let rulesMap = options.CustomRules |> rulesMap
    let ann = Schema.Annotator(options.Annotations)
    let defValues = Collections.Generic.Dictionary<string*string,obj>()
    let casesCache = Collections.Generic.Dictionary<string, Map<string,UnionCase>>()
    let getCasesMap (cases:UnionCase array) unionType =
        let unionName = Schema.nameFromType unionType
        match casesCache.TryGetValue unionName with
        | true, map -> map
        | _ ->
            let map =
                cases
                |> List.ofArray
                |> List.collect (fun case ->
                    let caseFullName = unionName + "." + case.CaseName
                    (caseFullName,case) ::
                        (ann.Record caseFullName
                            |> Option.map (fun r ->
                                r.Aliases
                                |> List.map (fun alias -> alias,case))
                            |> Option.defaultValue []))
                |> Map.ofList
            casesCache.[unionName] <- map
            map

    fun (json: Json) ->
        let rec parse (ti: TypeInfo) (json: Json) =
            match json,ti with
            | JString v, TypeInfo.String -> unbox v
            | JBool v, TypeInfo.Bool -> unbox v
            | JNumber v, TypeInfo.Byte -> v |> byte |> unbox
            | JNumber v, TypeInfo.Short -> v |> int16 |> unbox
            | JNumber v, TypeInfo.UInt16 -> v |> uint16 |> unbox
            | JNumber v, TypeInfo.Int32 -> v |> int |> unbox
            | JNumber v, TypeInfo.UInt32 -> v |> uint32 |> unbox
            | JNumber v, TypeInfo.Long -> v |> int64 |> unbox
            | JNumber v, TypeInfo.UInt64 -> v |> uint64 |> unbox
            | JNumber v, TypeInfo.Float32 -> v |> float32 |> unbox
            | JNumber v, TypeInfo.Float -> v |> unbox
            | JString v, TypeInfo.Array f ->    // "\uFF00AA124342"
              Array.unfold (fun idx ->
                if (idx * 2 + 4) <= v.Length then Some(v.Substring(idx * 2 + 2, 2)) else None
                |> Option.map (fun v -> Convert.ToByte(v, 16), (idx + 1))) 0
              |> unbox
            | JString v, TypeInfo.Enum f ->
                let (_, type') = f()
                try
                    Enum.Parse(type', v)
                with
                | err ->
                    ann.Enum (Schema.nameFromTypeInfo true ti)
                    |> Option.bind (fun r -> r.Default)
                    |> Option.bind (function JString v -> Enum.Parse(type', v) |> Some | _ -> None)
                    |> Option.defaultWith (fun () ->
                        if options.EvolutionTolerantMode then Schema.stub options.CustomRules ti |> parse ti
                        else failwithf "'%s' was not found in %s set ForwardCompatibleMode=true for assign stub value, %A" v (type'.Name) err)
            | JArray elements, TypeInfo.List f -> parseArray (f()) elements |> unbox
            | JArray elements, TypeInfo.Array f -> parseArray (f()) elements |> Array.ofList |> unbox
            | JArray elements, TypeInfo.ResizeArray f -> parseArray (f()) elements |> ResizeArray |> unbox
            | JArray elements, TypeInfo.Set f -> parseArray (f()) elements |> List.map unbox |> Set.ofList |> unbox
            | JArray elements, TypeInfo.HashSet f -> parseArray (f()) elements |> Collections.Generic.HashSet |> unbox
            | JArray elements, TypeInfo.Seq f -> parseArray (f()) elements |> unbox
            | JObject props, TypeInfo.Map f ->
                let _, valueTypeInfo = f()
                props
                |> Map.map (fun _ jsonValue -> parse valueTypeInfo jsonValue )
                |> unbox
            | JObject props, TypeInfo.Dictionary f ->
                let _, valueTypeInfo, _ = f()
                let dict = Collections.Generic.Dictionary<string, _>()
                props
                |> Map.iter(fun key jsonValue -> dict.Add(key, parse valueTypeInfo jsonValue))
                unbox dict
            | JObject props, TypeInfo.Record f ->
                let fields, recordType = f()
                let recordValues =
                    fields
                    |> Array.map (fun field -> fieldValue props (fun () -> Schema.nameFromType recordType) field.FieldName field.FieldType)
                FSharpValue.MakeRecord(recordType, recordValues)
            | JObject props, TypeInfo.Tuple f ->
                f()
                |> Array.mapi(fun idx itemTypeInfo ->
                    let fieldName = sprintf "Item%d" (idx+1)
                    fieldValue props (fun () -> Schema.nameFromTypeInfo true ti) fieldName itemTypeInfo)
                |> unbox
            | JObject props, TypeInfo.Union f ->
                let unionCases, unionType = f()
                let caseName, json = props |> Map.toSeq |> Seq.head
                match json with
                | JObject props ->
                    let map = getCasesMap unionCases unionType
                    map.TryFind caseName
                    |> Option.map (fun case ->
                        let unionFields =
                            case.Info.GetFields()
                            |> Array.mapi(fun idx pi ->
                                fieldValue props (fun () -> caseName) pi.Name case.CaseTypes.[idx])
                        FSharpValue.MakeUnion(case.Info, unionFields))
                    |> Option.defaultWith (fun () ->
                        if options.EvolutionTolerantMode then
                            Schema.stub options.CustomRules ti
                            |> parse ti
                        else failwithf "UnionCase:%s missed, set ForwardCompatibleMode=true for assign stub value" caseName)
                | wrong -> failwithf "Union case expected to be in json object, but %A" wrong
            | JNull, TypeInfo.Option f -> unbox None
            | JObject props, TypeInfo.Option f ->
                match props |> Map.toSeq |> Seq.tryHead with
                | Some (_,jsonValue) ->
                    let parsedOptional = unbox (parse (f()) jsonValue)
                    unbox Some parsedOptional
                | None -> failwith "Option case should be represented as jobject with one property"
            | JNumber v, TypeInfo.Decimal -> v |> decimal |> unbox
            | JString v, TypeInfo.Decimal -> unbox (decimal v)
            | JString v, TypeInfo.Guid -> Guid.Parse v |> unbox
            | JString v, TypeInfo.DateTime -> DateTime.Parse(v, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.RoundtripKind) |> unbox
            | JString v, TypeInfo.DateTimeOffset -> DateTimeOffset.Parse(v, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.RoundtripKind) |> unbox
            | JNumber v, TypeInfo.TimeSpan -> unbox (JS.Math.floor v)
            | JNumber v, TypeInfo.BigInt -> unbox (bigint (JS.Math.floor(v)))
            | JString v, TypeInfo.BigInt -> unbox unbox (Numerics.BigInteger.Parse v)
            | json, TypeInfo.Any f ->
                let type' = f()
                match rulesMap.TryFind type'.Name with
                | Some rule ->
                    parse rule.SurrogateTypeInfo json
                    |> rule.CastFromSurrogate
                | _ -> failwithf "Deserialization of the type is not supported: %A" (f())
            | wrong -> failwithf "Deserialization of the type is not supported: %A" wrong
        and parseArray elTypeInfo elements =
            elements |> List.map (parse elTypeInfo)
        and fieldValue (props:Map<string,Json>) recordNameF fieldName fieldTypeInfo =
            props.TryFind fieldName
            |> Option.map (parse fieldTypeInfo)
            |> Option.orElseWith (fun () ->
                let recordName = recordNameF()
                ann.Field recordName fieldName
                |> Option.bind (fun r ->
                    r.Aliases
                    |> List.choose (props.TryFind)
                    |> List.tryHead
                    |> Option.map (parse fieldTypeInfo)))
            |> Option.defaultWith (fun () ->
                let recordName = recordNameF()
                match defValues.TryGetValue ((recordName,fieldName)) with
                | true, v -> v
                | _ ->
                    let v =
                        ann.Field recordName fieldName
                        |> Option.bind (fun r ->
                            r.Default
                            |> Option.map (fun defJson ->
                                match fieldTypeInfo with
                                | TypeInfo.Union f ->
                                    let cases, unionType = f()
                                    [(Schema.nameFromType unionType) + "." + cases.[0].Info.Name, defJson]
                                    |> Map.ofList
                                    |> JObject
                                | _ -> defJson)
                            |> Option.map (parse fieldTypeInfo))
                        |> Option.defaultWith (fun () ->
                            if options.EvolutionTolerantMode then
                                Schema.stub options.CustomRules fieldTypeInfo
                                |> parse fieldTypeInfo
                            else failwithf "Record:%s Field:%s missed, set ForwardCompatibleMode=true for assign stub value" recordName fieldName)
                    defValues.[(recordName,fieldName)] <- v
                    v)

        parse ti json

let inline createDeserializer<'T> (options:DeserializationOptions): Json -> Result<'T, string> =
    let deserializer = createDeserializer' typeof<'T> options

    fun (json: Json) ->
        try
            deserializer json :?> 'T |> Ok
        with
        | err -> Error err.Message