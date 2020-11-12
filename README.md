# Fable.Avro

Fable implementation of Apache Avro

## Overview

The library generates an Avro schema by an F# type as well as serialize and deserialise an F# data in json encodings.
It works seamlessly with (Avro.FSharp)[https://github.com/usix79/Avro.FSharp].
Based on (Fable.SimpleJson)[https://github.com/Zaid-Ajaj/Fable.SimpleJson].

## Schema

To generate a schema, use Schema.generate:

```fsharp
val generate:
   customRules: list<CustomRule> ->     // custom rules for non supported types
   annotations: String           ->     // information about aliases and default values in json string
   type'      : Type                    // type to build schema of
             -> Result<Schema,SchemaError>
```

To read a schema from json string, use Schema.ofString:
```fsharp
val ofString:
   jsonString: string
            -> Schema
```
To write schema, use Schema.toString:
```fsharp
val toString:
   schema: Schema
        -> string
```


### Primitive types

| F# type | Avro type |
|---------|-----------|
| `string` | `string` |
| `bool` | `boolean` |
| `byte` | `int` |
| `short` | `int` |
| `int` | `int` |
| `uint` | `int` |
| `int16` | `int` |
| `int64` | `long` |
| `uint64` | `long` |
| `float32` | `float` |
| `float` | `double` |
| `byte array` | `bytes` |

Examples:
* `generate [] "" typeof<string>` generates: `{"type": "string"}`
* `generate [] "" typeof<bool>` generates: `{"type": "boolean"}`
* `generate [] "" typeof<int>` generates: `{"type": "int"}`
* `generate [] "" typeof<int64>` generates: `{"type": "long"}`
* `generate [] "" typeof<float32>` generates: `{"type": "float"}`
* `generate [] "" typeof<float>` generates: `{"type": "double"}`
* `generate [] "" typeof<byte array>` generates: `{"type": "bytes"}`

### Arrays

Following F# types are mapped to avro's array:
* `'T list`
* `'T array` (if `'T` is not `byte`)
* `ResizeArray<'T>` (`System.Collection.Generic.List<'T>`)
* `Set<'T>`
* `HashSet<'T>`
* `'T Seq` (`IEnumerable<'T>`)

Examples:
* `generate [] "" typeof<string list>` generates: `{"type": "array", "values": "string"}`
* `generate [] "" typeof<int array>` generates: `{"type": "array", "values": "int"}`
* `generate [] "" typeof<List<bool>>` generates: `{"type": "array", "values": "boolean"}`

### Maps

Following F# types are mapped to avro's map:
* `Map<string,'TValue>`
* `Dictionary<string,'TValue>`

Examples:
* `generate [] "" typeof<Map<string,string>>` generates: `{"type": "map", "values": "string"}`
* `generate [] "" typeof<Dictionary<string, int>>` generates: `{"type": "map", "values": "int"}`


### Enums

F# Enum is mapped to Avro's `enum`

Example:

```fsharp
type TestState =
    | Red = 3
    | Yellow = 2
    | Green = 1

generate [] typeof<TestState>
```
generated schema:
```json
{"type": "enum", "name": "TestState", "symbols": ["Green", "Yellow", "Red"]}
```

Symbols are ordered by its values.

### Records

F# records and tuples are mapped to Avro's `record`.

Example:

```fsharp
type SimpleRecord = {
    Id : int
    Name : string
    Version : int64}

generate [] "" typeof<SimpleRecord>
```
generated schema:
```json
{
    "type": "record",
    "name": "SimpleRecord",
    "fields" : [
        {"name": "Id", "type": "int"},
        {"name": "Name", "type": "string"},
        {"name": "Version", "type": "long"}
    ]
}
```

A tuple is mapped to Avro's `record` with fields `Item1`, `Item2` and so on.

Example:

`generate [] "" typeof<int*string>>` generates
```json
{
    "type": "record",
    "name": "Tuple_Of_Int32_And_String",
    "fields" : [
        {"name": "Item1", "type": "int"},
        {"name": "Item2", "type": "string"},
    ]
}
```

Generic records are also allowed:
```fsharp
type GenericRecord<'T> = {
    Value : 'T
}

generate [] "" typeof<GenericRecord<string>>
```
generated schema:
```json
{
    "type":"record",
    "name":"GenericRecord_Of_String",
    "fields":[{"name":"Value","type":"string"}]
}
```

### Unions

F# Discriminated Union is mapped to Avro's `union` of records, generated from the union's cases

Example:

```fsharp
type BinaryTree =
    | Leaf of value:string
    | Node of left: BinaryTree * right: BinaryTree

generate [] "" typeof<BinaryTree>
```
generated schema:
```json
{
    "type":[
        {
            "type":"record",
            "name":"Leaf",
            "fields":[
                {"name":"value","type":"string"}
            ]
        },
        {
            "type":"record",
            "name":"Node",
            "fields":[
                {"name":"left","type":["Leaf","Node"]},
                {"name":"right","type":["Leaf","Node"]}
            ]
        }
    ]
}
```

Option is mapped as `union` of `null` and the option's generic argument's type

Example:

`generate [] "" typeof<Option<float>>` generates `["null","double"]`

### Special cases

Following types are handled in special way

| F# type | Avro type | Description |
|---------|-----------|-------------|
| `Guid` |  `string` | |
| `Decimal` | `string` | |
| `BigInt` | `string` | |
| `DateTime` | `string` | ISO 8601 |
| `DateTimeOffset` | `string` | ISO 8601 |
| `TimeSpan` | `int` | milliseconds |
| `Uri` | `string` | |

## Logical types
NOT YET SUPPORTED

## Annotations

Some schema's attributes can not be evolved from F# type (default value and aliases).
Additional annotation is used for the purpose. Here is example of the annotation json.

```json
{
    "records": [{
        "name": "Foo.Bar.NewRecord",        // Full name of the record
        "aliases": ["Foo.Bar.OldRecord"],   // Aliases attributes in the record's schema
        "fields": [{
                "name": "Caption",          // Name of the record's field
                "aliases": ["Title", "Cap"] // Aliases atttibute in the field's schema
            },
            {
                "name": "Description",
                "aliases": [],
                "default": "Not Yet Described"  // Default value in the fied's schema
            }
        ]
    }],
    "enums": [{
        "name": "Foo.Bar.NewTestState",     // Full name of the enum
        "aliases": ["Foo.Bar.TestState"],   // Aliases attribute is the enum's schema
        "default": "Blue"                   // Default value in the enum's schema
    }]
}
```

You don't need to describe all enums, records and fields is an annotation, only those which should be enriched with additional attributes. Since tuples and DU cases are mapped to a record, you may define attibutes for them as well. Remember, that tupel's field name is like `Item1, Item2, Item3 ...`. DU case name is composed from name of the DU and name of the case.

## Names

According to avro specification, only `[A-Za-z0-9_]` symbols are allowed, some substitutions are performed.
Name of an enum is constructed from namespace and type's name. Name of a record also contains description of the generic type arguments.

Rule of the generation of the records name is describer is the following table:

| Precicate | Rule |
|------|---------|
| Is kind of array | `Array_Of_{ItemTypeName}` |
| Is kind of map | `Map_Of_{ValueTypeName}` |
| IsGenericType | `{TypeName}_Of_{GenericType1Name}_And_{GenericType2Name}_...` |
| Is Result<OkType,ErrType> | `Result_Of_{OkTypeName}_{ErrTypeName}` |
| Is Some<T>| `Nullable_{T}` |
| Is Tuple| `Tuple_Of_{Item1TypeName}_{Item2TypeName}_...` |
| Is DU case | `{DU Type Name}.{CaseName}`
| Is starting from `System.` | remove `System.` |

Examples of record names:
* `Result_Of_Int64_And_String.Ok`
* `Tuple_Of_Int32_And_String`
* `Foo.Bar.GenericRecord_Of_Nullable_String`

# Json Serde

To create serializer use:
```fsharp
val createSerializer:
   customRules: list<CustomRule>
             -> 'T -> Result<Json,string>
```

To create deserialized use:
```fsharp
val createDeserializer:
   customRules: list<CustomRule> ->
   annotations: string
             -> Json -> Result<'T,string>
```

Here is basic example:

```fsharp
    let orig:MyType = createInstance()

    let serializer = JsonSerde.createSerializer<MyType> case.InstanceType []
    let deserializer = JsonSerde.createDeserializer<MyType> case.ExpectedType [] myTypeAnnotations

    match serializer case.Instance with
    | Ok json ->
        match deserializer json with
        | Ok copy -> Expect.equal orig copy "copy shoud be equal to original"
        | Error err -> failwithf "Error: %A" err
    | Error err -> failwithf "Error: %A" err
```

# Forward Compatibility of Descriminated Unions

According to Avro standard, adding a new type at a union is a non forward compatible change ([see](https://avro.apache.org/docs/current/spec.html#Schema+Resolution)).

Let's pretend that first version of our domain looks like:
```fsharp
type DomainUnion =
    | Case1
    | Case2

type DomainRecord = {
    Union : DomainUnion
}
```

Eventually, version 2 is evolved:
```fsharp
type DomainUnion =
    | Case1
    | Case2
    | Case3

type DomainRecord = {
    Union : DomainUnion
}
```

According to Avro standard, microservices that use old schema (version 1) should get an error trying deserialize a `DomainRecord` with `Case3`. This is big obstacle for evolution of the algebraic types. Therefore the library substitutes unknown case with default value for a record field.

For example, having following domain:

```fsharp
type UnionV1 =
    | UnknownCase
    | Case1

type RecordV1 = {
    Union : UnionV1
}

type UnionV2 =
    | UnknownCase
    | Case1
    | Case2
    | Case3

type RecordV2 = {
    Union : UnionV2
}
```

with annotation

```json
    """{
        "records": [
            {
                "name": "Foo.Bar.RecordV1",
                "fields": [
                    {"name": "Union", "aliases": [], "default": {"Foo.Bar.UnionV1.UnknownCase": {}}}
                ]
            }
        ]
    }"""
```

`{Union = Case3}:RecordV2` will be deserialized to `{Union = UnionV1.UnknownCase}:RecordV1` if `RecordV1` is target type.

Also unknown cases are skipped during reading encoded arrays and maps.

# Custom Rules

It is possible to customize processing of a particular type. In that case `CustomRule` should be created.
```fsharp
type CustomRule = {
    TargetType: System.Type;
    Schema: string
    SerializationCast: obj -> obj
    DeserializationCast: obj -> obj
}
```
There are several build in rules:
* `Guid` is mapped to byte array
* `Uri` is mapped to string
* `DateTime` is mapped to ISO 8601 string
* `DateTimeOffset` is mapped to ISO 8601 string
* `TimeSpan` is mapped to ISO 8601 string

Array with custom rules should be passed as first argument to `Schema.generate`.

# Examples
More examples of complex types and corresponging schemas is available in the [SchemaTests.fs](https://github.com/usix79/Avro.FSharp/blob/main/test/Avro.FSharp.Tests/SchemaTests.fs).

See [CustomRule.fs](https://github.com/usix79/Avro.FSharp/blob/main/src/Avro.FSharp/CustomRule.fs) for details of the implementation of the custom rules.

An implementation of Kafka producer and consumer, working with SchemaRegistry could be found [here](https://github.com/usix79/Avro.FSharp/tree/main/examples/KafkaSerde). (*The example is supposed you have an account in confluent.cloud. Change configuration code if you have on-premise installation.*)