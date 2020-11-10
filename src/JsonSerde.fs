module Fable.Avro.JsonSerde

open System
open FSharp.Reflection
open Fable.SimpleJson
open Fable.Core

let private rulesMap (rules:CustomRule list) =
    rules
    |> List.map(fun rule ->
        rule.InstanceType.Name,
        {|  SurrogateTypeInfo = createTypeInfo rule.SurrogateType
            SerializationCast = rule.SerializationCast
            DeserializationCast = rule.DeserializationCast|})
    |> Map.ofList

let inline createSerializer'(type': Type) (customRules:CustomRule list): obj -> Json =
    let ti = createTypeInfo type'
    let rulesMap = customRules @ CustomRule.buidInRules |> rulesMap

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
            | TypeInfo.List f -> value |> unbox<obj list> |> (parseArray(f()))
            | TypeInfo.Array f -> value |> unbox<obj []> |> List.ofArray |> (parseArray(f()))
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
                [usedCaseInfo.Name, caseRecord]
                |> Map.ofList
                |> JObject
            | TypeInfo.Option f ->
                match unbox<obj option> value with
                | None -> JNull
                | Some v ->
                    let typeInfo = f()
                    [Schema.nameFromTypeInfo true typeInfo, parse v typeInfo] |> Map.ofList |> JObject
            | TypeInfo.Decimal -> unbox value |> float |> JNumber
            | TypeInfo.Guid -> (unbox<Guid> value).ToString() |> JString
            | TypeInfo.DateTime -> (unbox<DateTime> value).ToString("O", Globalization.CultureInfo.InvariantCulture) |> JString
            | TypeInfo.DateTimeOffset -> (unbox<DateTimeOffset> value).ToString("O", Globalization.CultureInfo.InvariantCulture) |> JString
            | TypeInfo.TimeSpan -> unbox<int> value |> float |> JNumber
            | TypeInfo.Any f ->
                let type' = f()
                match rulesMap.TryFind type'.Name with
                | Some rule -> parse (rule.SerializationCast value) rule.SurrogateTypeInfo
                | _ -> failwithf "Serialization of the type is not supported: %A" type'
            | wrong -> failwithf "Serialization of the type is not supported: %A" wrong
        and parseArray elTypeInfo elements  =
            elements |> List.map (fun el -> parse el elTypeInfo) |> JArray

        parse value ti

let inline createSerializer<'T> (customRules:CustomRule list): 'T -> Result<Json, string> =
    let serializer = createSerializer' typeof<'T> customRules
    fun (value:'T) ->
        try
            serializer value |> Ok
        with
        | err -> Error err.Message

let inline createDeserializer' (type': Type) (customRules:CustomRule list): Json -> obj =
    let ti = createTypeInfo type'
    let rulesMap = customRules @ CustomRule.buidInRules |> rulesMap

    fun (json: Json) ->
        let rec parse (json: Json) (ti: TypeInfo) =
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
            | JString v, TypeInfo.Enum f ->
                let (_, type') = f()
                Enum.Parse(type', v)
                // TODO: default value
            | JArray elements, TypeInfo.List f -> parseArray (f()) elements |> unbox
            | JArray elements, TypeInfo.Array f -> parseArray (f()) elements |> Array.ofList |> unbox
            | JArray elements, TypeInfo.ResizeArray f -> parseArray (f()) elements |> ResizeArray |> unbox
            | JArray elements, TypeInfo.Set f -> parseArray (f()) elements |> List.map unbox |> Set.ofList |> unbox
            | JArray elements, TypeInfo.HashSet f -> parseArray (f()) elements |> Collections.Generic.HashSet |> unbox
            | JArray elements, TypeInfo.Seq f -> parseArray (f()) elements |> unbox

            | JObject props, TypeInfo.Map f ->
                let _, valueTypeInfo = f()
                props
                |> Map.map (fun _ jsonValue -> parse jsonValue valueTypeInfo)
                |> unbox
            | JObject props, TypeInfo.Dictionary f ->
                let _, valueTypeInfo, _ = f()
                let dict = Collections.Generic.Dictionary<string, _>()
                props
                |> Map.iter(fun key jsonValue -> dict.Add(key, parse jsonValue valueTypeInfo))
                unbox dict
            | JObject props, TypeInfo.Record f ->
                let fields, recordType = f()
                let recordValues =
                    fields
                    |> Array.map (fun field ->
                        props.TryFind field.FieldName
                        |> Option.map (fun value -> parse value field.FieldType)
                        |> Option.defaultValue null
                    )
                FSharpValue.MakeRecord(recordType, recordValues)
            | JObject props, TypeInfo.Tuple f ->
                f()
                |> Array.mapi(fun idx itemTypeInfo ->
                    props.TryFind (sprintf "Item%d" (idx+1))
                    |> Option.map (fun itemJson -> parse itemJson itemTypeInfo)
                    |> Option.defaultValue null)
                |> unbox
            | JObject props, TypeInfo.Union f ->
                let unionCases, unionType = f()
                let caseName, json = props |> Map.toSeq |> Seq.head
                match json with
                | JObject props ->
                    match unionCases |> Array.tryFind (fun case -> case.CaseName = caseName) with
                    | Some case ->
                        let unionFields =
                            case.Info.GetFields()
                            |> Array.mapi(fun idx pi ->
                                match props.TryFind pi.Name with
                                | Some json -> parse json case.CaseTypes.[idx]
                                | None -> null)
                        FSharpValue.MakeUnion(case.Info, unionFields)
                    | None -> failwithf "Case not found %A" caseName
                | wrong -> failwithf "Union case expected to be in json object, but %A" wrong
            | JNull, TypeInfo.Option f -> unbox None
            | JObject props, TypeInfo.Option f ->
                match props |> Map.toSeq |> Seq.tryHead with
                | Some (_,jsonValue) ->
                    let parsedOptional = unbox (parse jsonValue (f()))
                    unbox Some parsedOptional
                | None -> failwith "Option case should be represented as jobject with one property"
            | JNumber v, TypeInfo.Decimal -> v |> decimal |> unbox
            | JString v, TypeInfo.Guid -> Guid.Parse v |> unbox
            | JString v, TypeInfo.DateTime -> DateTime.Parse(v, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.RoundtripKind) |> unbox
            | JString v, TypeInfo.DateTimeOffset -> DateTimeOffset.Parse(v, Globalization.CultureInfo.InvariantCulture, Globalization.DateTimeStyles.RoundtripKind) |> unbox
            | JNumber v, TypeInfo.TimeSpan -> unbox (JS.Math.floor v)

            | json, TypeInfo.Any f ->
                let type' = f()
                match rulesMap.TryFind type'.Name with
                | Some rule ->
                    parse json rule.SurrogateTypeInfo
                    |> rule.DeserializationCast
                | _ -> failwithf "Deserialization of the type is not supported: %A" (f())
            | wrong -> failwithf "Deserialization of the type is not supported: %A" wrong
        and parseArray elTypeInfo elements =
            elements |> List.map (fun el -> parse el elTypeInfo)

        parse json ti

let inline createDeserializer<'T> (customRules:CustomRule list): Json -> Result<'T, string> =
    let deserializer = createDeserializer' typeof<'T> customRules

    fun (json: Json) ->
        try
            deserializer json :?> 'T |> Ok
        with
        | err -> Error err.Message