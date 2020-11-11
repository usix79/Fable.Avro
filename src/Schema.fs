namespace Fable.Avro

open System
open System.Collections.Generic
open Fable.SimpleJson

type Schema =
    | Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | Bytes
    | String
    | Record of RecordSchema
    | Enum of EnumSchema
    | Array of ArraySchema
    | Map of MapSchema
    | Union of Schema array
    | Fixed of FixedSchema
    | Decimal of DecimalSchema
and RecordSchema = {Name: string; Aliases: string list; Fields: List<RecordField>}
and RecordField = {Name: string; Aliases: string list; Type: Schema; Default: Json option}
and EnumSchema = {Name: string; Aliases: string list; Symbols: string array; Default: Json option}
and ArraySchema = {Items: Schema; Default: Json option}
and MapSchema = {Values: Schema; Default: Json option}
and FixedSchema = {Name: string; Aliases: string list; Size: int}
and DecimalSchema = {Precision: int; Scale: int}

type SchemaError =
    | AggregateError of SchemaError list
    | NotSupportedType of Type
    | NotSupportedTypeInfo of TypeInfo

type Cache<'TKey,'TValue when 'TKey : equality>() =
    inherit Dictionary<'TKey, 'TValue>()
    member this.AddValue key value' = this.[key] <- value'; value'
    member this.TryFind key = match this.TryGetValue key with true, schema -> Some schema | _ -> None

module Schema =

    let private canonicalName ns (name:string) =
        match name.LastIndexOf "." with
        | -1 -> ns, if ns <> "" then ns + "." + name else name
        | idx -> name.Substring(0, idx), name

    let private getStringProp (fields:Map<string,Json>) propertyName =
        match fields.TryFind propertyName with
        | Some (JString v) -> v
        | _ -> failwithf "string property '%s' is absent in %A" propertyName fields

    let private getNumberProp (fields:Map<string,Json>) propertyName =
        match fields.TryFind propertyName with
        | Some (JNumber v) -> v
        | _ -> failwithf "string property '%s' is absent in %A" propertyName fields

    let private getStringList = List.choose (function JString v -> Some v | _ -> None)

    let private getName ns (props:Map<string,Json>) =
        let ns = match props.TryFind "namespace" with Some (JString v) -> v | _ -> ns
        canonicalName ns (getStringProp props "name")

    let private getFullName (props:Map<string,Json>) =
        getName "" props |> snd

    let private getAliases ns (props:Map<string,Json>) =
        match props.TryFind "aliases" with
        | Some (JArray list) ->  getStringList list |> List.map ((canonicalName ns) >> snd)
        | _ -> []

    let private getSymbols (props:Map<string,Json>) =
        match props.TryFind "symbols" with
        | Some (JArray list) -> getStringList list |> Array.ofList
        | _ -> [||]

    let private getDefault (props:Map<string,Json>) = props.TryFind "default"

    let ofString (jsonString:string) =
        let cache = Cache<string,Schema>()

        let rec parse ns = function
            | JNull -> Null
            | JString "null" -> Null
            | JString "string" -> String
            | JString "boolean" -> Boolean
            | JString "int" -> Int
            | JString "long" -> Long
            | JString "float" -> Float
            | JString "double" -> Double
            | JString "bytes" -> Bytes
            | JString typeName ->
                match cache.TryGetValue(canonicalName ns typeName |> snd) with
                | true, schema -> schema
                | _ -> failwithf "Unknown type: %s" typeName
            | JObject props ->
                match props.["type"] with
                | JString "array" -> Array { Items = parse ns props.["items"]; Default = getDefault props}
                | JString "map" -> Map { Values = parse ns props.["values"]; Default = getDefault props}
                | JString "enum" ->
                    let ns, name = getName ns props
                    Enum {  Name = name;
                            Aliases = getAliases ns props;
                            Symbols = getSymbols props;
                            Default = getDefault props}
                    |> cache.AddValue name
                | JString "record" ->
                    let ns, name = getName ns props
                    let fields = List<RecordField>()
                    let schema =
                        Record {Name = name; Aliases = getAliases ns props; Fields = fields}
                        |> cache.AddValue name
                    match props.["fields"] with
                    | JArray list ->
                        list |> List.iter (fun fieldJson ->
                            match fieldJson with
                            | JObject props ->
                                {   Name = getStringProp props "name";
                                    Aliases = getAliases "" props;
                                    Type = parse ns props.["type"];
                                    Default = getDefault props}
                                |> fields.Add
                            | wrongJson -> failwithf "wrong json for field item property %A" wrongJson)
                        schema
                    | wrongJson -> failwithf "wrong json for 'fields' property %A" wrongJson
                | JString "fixed" ->
                    let ns, name = getName ns props
                    Fixed { Name = name; Aliases = getAliases ns props; Size = getNumberProp props "size" |> int}
                    |> cache.AddValue name
                | JString "bytes" when props.TryFind "logicalType" = Some (JString "decimal") ->
                    Decimal {Precision = getNumberProp props "precision" |> int; Scale = getNumberProp props "scale" |> int}
                | json -> parse ns json
            | JArray list -> list |> List.map (parse ns) |> Array.ofList |> Union
            | wrongJson -> failwithf "wrong json %A" wrongJson

        SimpleJson.parse jsonString
        |> parse ""


    type internal Annotator(jsonString:String) =
        let json = SimpleJson.tryParse jsonString |> Option.defaultValue (JObject Map.empty)

        let getProps = function JObject props -> props | wrong -> failwithf "expected object but get %A" wrong

        let getList name (props:Map<string,Json>) =
            props.TryFind name |> Option.bind (function | JArray list -> Some list | _ -> None)

        let field (props:Map<string,Json>) =
            {|  Name = getFullName props; Aliases = getAliases "" props; Default = props.TryFind "default"  |}

        let records,enums,decimals =
            getProps json
            |> getList "records"
            |> Option.map (List.map (fun json ->
                let props = getProps json
                {|  Name = getFullName props
                    Aliases = getAliases "" props
                    Fields =  getList "fields" props |> Option.map (List.map (getProps >> field)) |})),
            getProps json
            |> getList "enums"
            |> Option.map (List.map (fun json ->
                let props = getProps json
                {|  Name = getFullName props
                    Aliases = getAliases "" props
                    Default = props.TryFind "default" |})),
            getProps json
            |> getList "decimals"
            |> Option.map (List.map (fun json ->
                let props = getProps json
                {|  RecordName = getStringProp props "record"
                    FieldName = getStringProp props "field"
                    Scale = getNumberProp props "scale" |> int|}))

        member _.Enum name =
            enums
            |> Option.bind (List.tryFind (fun r -> r.Name = name))

        member _.Record name =
            records
            |> Option.bind (List.tryFind (fun r -> r.Name = name))

        member this.Field recordName fieldName =
            this.Record recordName
            |> Option.bind (fun r ->
                r.Fields
                |> Option.bind (List.tryFind (fun fr -> fr.Name = fieldName)))

        member _.Decimal recordName fieldName =
            decimals |> Option.bind(List.tryFind (fun r -> r.RecordName = recordName && r.FieldName = fieldName))

    let rec nameFromTypeInfo isRoot = function
        | TypeInfo.String -> if isRoot then "string" else "String"
        | TypeInfo.Bool -> if isRoot then "boolean" else "Boolean"
        | TypeInfo.Int32 -> if isRoot then "int" else "Int32"
        | TypeInfo.Long -> if isRoot then "long" else "Int64"
        | TypeInfo.Float32 -> if isRoot then "float" else "Float"
        | TypeInfo.Float -> if isRoot then "double" else "Double"
        | TypeInfo.Decimal -> if isRoot then "bytes" else "Decimal"
        | TypeInfo.Array f
        | TypeInfo.ResizeArray f
        | TypeInfo.HashSet f
        | TypeInfo.Set f
        | TypeInfo.Seq f -> "Array_Of_" + nameFromTypeInfo false (f())
        | TypeInfo.List f ->
            match f() with
            | TypeInfo.Byte when isRoot -> "bytes"
            | _ -> "Array_Of_" + nameFromTypeInfo false (f())
        | TypeInfo.Map f ->
            let (_, valueTypeInfo) = f()
            "Map_Of_" + nameFromTypeInfo false valueTypeInfo
        | TypeInfo.Dictionary f ->
            let (_, valueTypeInfo, _) = f()
            "Map_Of_" + nameFromTypeInfo false valueTypeInfo
        | TypeInfo.Enum f ->
            let _, type' = f()
            let name = type'.FullName.Replace('+','.')
            name.Substring(0, name.IndexOf("["))
        | TypeInfo.Record f ->
            let _, type' = f()
            nameFromType type'
        | TypeInfo.Tuple f ->
            "Tuple_Of_" + ((f() |> Array.map (nameFromTypeInfo false)) |> (String.concat "_And_"))
        | TypeInfo.Option f ->
            "Nullable_" + nameFromTypeInfo false (f())
        | TypeInfo.Union f ->
            let _, type' = f()
            nameFromType type'
        | TypeInfo.Any f -> nameFromType (f())
        | TypeInfo.Guid -> if isRoot then "string" else "Guid"
        | TypeInfo.DateTime  -> if isRoot then "string" else "DateTime"
        | TypeInfo.DateTimeOffset -> if isRoot then "string" else "DateTimeOffset"
        | TypeInfo.TimeSpan -> if isRoot then "int" else "TimeSpan"
        | TypeInfo.BigInt -> if isRoot then "string" else "BigInt"
        | wrongTypeInfo -> failwithf "Name for the type is not supported: %A" wrongTypeInfo
    and nameFromType (type':Type) : string =
        let name = type'.FullName.Replace('+','.')
        let name = if name.StartsWith "System." then name.Substring("System.".Length) else name
        let name =
            match type' with
            | t when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Result<_,_>> -> "Result`"
            | _ -> name
        match name.IndexOf('`') with
        | -1 -> name
        | idx ->
            type'.GetGenericArguments()
            |> Array.map (createTypeInfo >> (nameFromTypeInfo false))
            |> String.concat "_And_"
            |> (sprintf "%s_Of_%s" (name.Substring(0, idx)))

    let rec private traverse f list =

        let apply fResult xResult =
            match fResult,xResult with
            | Ok f, Ok x -> f x |> Ok
            | Error errs, Ok _ -> Error errs
            | Ok f, Error errs -> Error errs
            | Error errs1, Error errs2 -> Error (errs1 @ errs2)

        let (<*>) = apply
        let cons head tail = head :: tail

        match list with
        | [] -> Ok []
        | head::tail -> Ok cons <*> (f head |> Result.mapError List.singleton) <*> (traverse f tail)

    let generate (customRules:CustomRule list) (annotations:String) (type':Type) : Result<Schema,SchemaError> =
        let cache = Cache<string, Schema>()
        let annotator = Annotator(annotations)
        let typeInfo = createTypeInfo type'
        let customRules = customRules @ CustomRule.buidInRules

        let rec gen (ti:TypeInfo) : Result<Schema,SchemaError> =
            match ti with
            | TypeInfo.String -> String |> Ok
            | TypeInfo.Bool -> Boolean |> Ok
            | TypeInfo.Int32 -> Int |> Ok
            | TypeInfo.Long -> Long |> Ok
            | TypeInfo.Float32 -> Float |> Ok
            | TypeInfo.Float -> Double |> Ok
            | TypeInfo.Decimal -> Decimal {Precision=29; Scale=14} |> Ok
            | TypeInfo.Array f
            | TypeInfo.ResizeArray f
            | TypeInfo.Set f
            | TypeInfo.HashSet f
            | TypeInfo.Seq f
            | TypeInfo.List f ->
                match f() with
                | TypeInfo.Byte -> Bytes |> Ok
                | ti -> gen ti |> Result.map (fun schema -> Array {Items = schema; Default = None })
            | TypeInfo.Map f ->
                let (_, valueTypeInfo) = f()
                gen valueTypeInfo |> Result.map (fun schema ->  Map {Values = schema; Default = None })
            | TypeInfo.Dictionary f ->
                let (_, valueTypeInfo, _) = f()
                gen valueTypeInfo |> Result.map (fun schema ->  Map {Values = schema; Default = None })
            | TypeInfo.Enum f ->
                let _, type' = f()
                let name = nameFromTypeInfo true ti
                Enum
                  { Name = name
                    Aliases = annotator.Enum name |> Option.map (fun r -> r.Aliases) |> Option.defaultValue []
                    Symbols =
                        [|for obj in Enum.GetValues type' do obj|]
                        |> Array.sortBy (fun obj -> Int32.Parse (obj.ToString()))
                        |> Array.map (fun obj -> Enum.GetName(type', obj))
                    Default =
                        annotator.Enum name
                        |> Option.bind (fun r -> r.Default)}
                |> cache.AddValue name
                |> Ok
            | TypeInfo.Record f ->
                let fieldsInfo, type' = f()
                genRecord
                    (nameFromTypeInfo true ti)
                    (fieldsInfo
                        |> Array.map (fun fi -> fi.FieldName,fi.FieldType)
                        |> List.ofArray)
            | TypeInfo.Tuple f ->
                genRecord
                    (nameFromTypeInfo true ti)
                    (f()
                        |> Array.mapi (fun idx ti -> sprintf"Item%d" (idx+1),ti)
                        |> List.ofArray)
            | TypeInfo.Option f ->
                gen (f())
                |> Result.bind (fun someSchema -> Union [|Null; someSchema|] |> Ok)
            | TypeInfo.Union f ->
                let casesInfo, type' = f()
                casesInfo
                |> List.ofArray
                |> traverse (fun case ->
                    genRecord
                        (nameFromType type' + "." + case.CaseName)
                        (Array.zip (case.Info.GetFields()) case.CaseTypes
                            |> Array.map (fun (pi,t) -> pi.Name,t)
                            |> List.ofArray))
                |> Result.map (Array.ofList >> Union)
                |> Result.mapError AggregateError
            | TypeInfo.Byte -> Int |> Ok
            | TypeInfo.Short -> Int |> Ok
            | TypeInfo.Guid -> Fixed {Name = "guid"; Aliases = []; Size=16} |> Ok
            | TypeInfo.DateTime -> String |> Ok
            | TypeInfo.DateTimeOffset -> String |> Ok
            | TypeInfo.TimeSpan -> String |> Ok
            | TypeInfo.Any f ->
                let type' = f()
                match customRules |> List.tryFind (fun t -> type' = t.InstanceType) with
                | Some rule -> createTypeInfo rule.SurrogateType |> gen
                | None -> NotSupportedType type' |> Error
            | ti -> NotSupportedTypeInfo ti |> Error
        and genRecord recordName (fieldsInfo:(string*TypeInfo) list) =
            match cache.TryFind recordName with
            | Some schema -> schema |> Ok
            | None ->
                let fields = List<RecordField>()
                let schema =
                    Record { Name = recordName
                             Aliases =
                                annotator.Record recordName
                                |> Option.map (fun r -> r.Aliases)
                                |> Option.defaultValue []
                             Fields = fields}
                cache.[recordName] <- schema  // create schema in advance for using it in recursive types

                fieldsInfo
                |> traverse (fun (fieldName,typeInfo) ->
                    gen typeInfo
                    |> Result.map (fun fieldSchema ->
                        let fieldSchema =
                            match fieldSchema with
                            | Decimal s ->
                                annotator.Decimal recordName fieldName
                                |> Option.map (fun a -> {s with Scale = a.Scale})
                                |> Option.defaultValue s
                                |> Decimal
                            | _ -> fieldSchema
                        let defValue =
                            match fieldSchema with
                            | Union [|Null;_|] -> Some JNull
                            | _ -> None
                        match annotator.Field recordName fieldName with
                        | Some a -> { Name = fieldName; Aliases = a.Aliases; Type = fieldSchema; Default = a.Default}
                        | None -> { Name = fieldName; Aliases = []; Type = fieldSchema; Default = defValue}))
                |> Result.map (fun recordFields -> fields.AddRange(recordFields); schema)
                |> Result.mapError AggregateError

        gen typeInfo

    let toString (schema:Schema) =
        let cache = Cache<string,Schema>()

        let typeField (typeName:string) = "type", JString typeName
        let nameField (name:string) = "name", JString name
        let arrayField (name:string) = function
            | [||] -> None
            | (values:string array) -> (name, values |> Array.map JString |> List.ofArray |> JArray) |> Some

        let aliasesField aliases = arrayField "aliases" (aliases |> Array.ofList)
        let defaultField = Option.map (fun v -> "default", v)

        let rec parse = function
            | Null -> JNull
            | Boolean -> JString "boolean"
            | Int -> JString "int"
            | Long -> JString "long"
            | Float -> JString "float"
            | Double -> JString "double"
            | Bytes -> JString "bytes"
            | String -> JString "string"
            | Array schema ->
                [   typeField "array"
                    "items", parse schema.Items
                    yield! defaultField schema.Default |> Option.toArray
                ] |> Map.ofList |> JObject
            | Map schema ->
                [   typeField "map"
                    "values", parse schema.Values
                    yield! defaultField schema.Default |> Option.toArray
                ] |> Map.ofList |> JObject
            | Enum schema when cache.ContainsKey schema.Name -> JString schema.Name
            | Enum schema ->
                cache.[schema.Name] <- Enum schema
                [   typeField "enum"
                    nameField schema.Name
                    yield! aliasesField schema.Aliases |> Option.toArray
                    yield! arrayField "symbols" schema.Symbols |> Option.toArray
                    yield! defaultField schema.Default |> Option.toArray
                ] |> Map.ofList |> JObject
            | Record schema when cache.ContainsKey schema.Name -> JString schema.Name
            | Record schema ->
                cache.[schema.Name] <- Record schema
                [   typeField "record"
                    nameField schema.Name
                    yield! aliasesField schema.Aliases |> Option.toArray
                    "fields",
                        [ for field in schema.Fields do
                            [   nameField field.Name
                                yield! aliasesField field.Aliases |> Option.toArray
                                "type", parse field.Type
                                yield! defaultField field.Default |> Option.toArray
                            ]|> Map.ofList |> JObject
                        ] |> JArray
                ] |> Map.ofList |> JObject
            | Union schemas -> schemas |> Array.map parse |> List.ofArray |> JArray
            | Fixed schema ->
                cache.[schema.Name] <- Fixed schema
                [   typeField "fixed"
                    nameField schema.Name
                    yield! aliasesField schema.Aliases |> Option.toArray
                    "size", JNumber (float schema.Size)
                ] |> Map.ofList |> JObject
            | Decimal schema ->
                    [   typeField "bytes"
                        "logicalType", JString "decimal"
                        "precision", JNumber (float schema.Precision)
                        "scale", JNumber (float schema.Scale)
                    ] |> Map.ofList |> JObject
            | wrong -> failwithf "wrong case %A" wrong

        parse schema
        |> SimpleJson.toString