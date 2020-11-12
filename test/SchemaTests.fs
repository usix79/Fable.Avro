﻿module SchemaTests

open Fable.Mocha
open Fable.Avro

open Foo.Bar

let expectSchemasEqual (actual:Schema) (expected:Schema) =
    "Schemas should be equal"
    |> Expect.equal (actual |> Schema.toString) (expected |> Schema.toString)

let generateSchema' ann type' =
    match Schema.generate [] ann type' with
    | Ok schema -> schema
    | Error err -> failwithf "Schema error: %A" err

let generateSchema = generateSchema' ""

let schemaTests =
    [
        testList "Primitive" [
            let pairs = [
                "string", typeof<string>
                "boolean", typeof<bool>
                "int", typeof<int>
                "long", typeof<int64>
                "float", typeof<float32>
                "double", typeof<float>
                "bytes", typeof<byte array>
            ]
            for typeName,type' in pairs ->
                testCase typeName <| fun _ ->
                    expectSchemasEqual
                        <| generateSchema type'
                        <| Schema.ofString (sprintf "{\"type\": \"%s\"}" typeName)
        ]

        testList "Array" [
            testCase "List" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<string list>
                    <| Schema.ofString """{"type": "array","items": "string"}"""
            testCase "Array" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<int array>
                    <| Schema.ofString """{"type": "array","items": "int"}"""
            testCase "Generic.List" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<System.Collections.Generic.List<bool>>
                    <| Schema.ofString """{"type": "array","items": "boolean"}"""
        ]

        testList "Map" [
            testCase "Map" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<Map<string,string>>
                    <| Schema.ofString """{"type": "map", "values": "string"}"""
            testCase "Generic.Dictionary" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<System.Collections.Generic.Dictionary<string, int>>
                    <| Schema.ofString """{"type": "map", "values": "int"}"""
        ]

        testList "Enum" [
            testCase "TestState" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<TestState>
                    <| Schema.ofString """{"type": "enum", "name": "Foo.Bar.TestState", "symbols": ["Green", "Yellow", "Red"]}"""
        ]

        testList "Record" [
            testCase "SimpleRecord" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<SimpleRecord>
                    <| Schema.ofString """{
                        "type": "record",
                        "name": "Foo.Bar.SimpleRecord",
                        "fields" : [
                            {"name": "Id", "type": "int"},
                            {"name": "Name", "type": "string"},
                            {"name": "Version", "type": "long"}
                        ]
                    }"""
            testCase "ParentRecord" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<ParentRecord>
                    <| Schema.ofString """{
                        "type":"record",
                        "name":"Foo.Bar.ParentRecord",
                        "fields":[
                            {
                                "name":"Chield1",
                                "type":{
                                    "type":"record",
                                    "name":"Foo.Bar.SimpleRecord",
                                    "fields":[
                                        {"name":"Id","type":"int"},
                                        {"name":"Name","type":"string"},
                                        {"name":"Version","type":"long"}]
                                }
                            },
                            {
                                "name":"Chield2",
                                "type":"SimpleRecord"
                            }
                        ]}"""
        ]

        testList "Union" [
            testCase "Result" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<Result<int64,string>>
                    <| Schema.ofString """[
                            {"type":"record","name":"Ok","namespace":"Result_Of_Int64_And_String","fields":[
                                {"name":"ResultValue","type":"long"}
                            ]},
                            {"type":"record","name":"Error","namespace":"Result_Of_Int64_And_String","fields":[
                                {"name":"ErrorValue","type":"string"}]
                            }
                        ]"""
            testCase "BinaryTree" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<BinaryTree>
                    <| Schema.ofString """{
                        "type":[
                            {
                                "type":"record",
                                "name":"Leaf",
                                "namespace":"Foo.Bar.BinaryTree",
                                "fields":[
                                    {"name":"value","type":"string"}
                                ]
                            },
                            {
                                "type":"record",
                                "name":"Node",
                                "namespace":"Foo.Bar.BinaryTree",
                                "fields":[
                                    {"name":"left","type":["Leaf","Node"]},
                                    {"name":"right","type":["Leaf","Node"]}
                                ]
                            }
                        ]}"""
        ]

        testList "Tuple" [
            testCase "Tuple" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<int*string>
                    <| Schema.ofString """{
                        "type": "record",
                        "name": "Tuple_Of_Int32_And_String",
                        "fields" : [
                            {"name": "Item1", "type": "int"},
                            {"name": "Item2", "type": "string"}
                        ]
                    }"""
        ]

        testList "Nullable" [
            testCase "Option" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<Option<float>>
                    <| Schema.ofString """["null","double"]"""
            testCase "Option in a Record" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<RecordWithOption>
                    <| Schema.ofString """{
                        "type":"record",
                        "name":"Foo.Bar.RecordWithOption",
                        "fields":[
                            {"name":"Id","type":"int"},
                            {"name":"Id2","type":["null","int"],"default":null}
                        ]
                    }"""
        ]

        testList "GenericNaming" [
            testCase "Record<Option>" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<GenericRecord<Option<string>>>
                    <| Schema.ofString """{
                        "type":"record",
                        "name":"Foo.Bar.GenericRecord_Of_Nullable_String",
                        "fields":[
                            {"name":"Value","type":["null","string"],"default":null}
                        ]
                    }"""
            testCase "Record<List<Option>>" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<GenericRecord<List<Option<string>>>>
                    <| Schema.ofString """{
                        "type":"record",
                        "name":"Foo.Bar.GenericRecord_Of_Array_Of_Nullable_String",
                        "fields":[
                            {"name":"Value","type":{"items":["null","string"],"type":"array"}}
                        ]
                    }"""
            testCase "Record<Map<string, string list>>>" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<GenericRecord<Map<string, string list>>>
                    <| Schema.ofString """{
                        "type":"record",
                        "name":"Foo.Bar.GenericRecord_Of_Map_Of_Array_Of_String",
                        "fields":[
                            {"name":"Value","type":{"values":{"items":"string","type":"array"},"type":"map"}}
                        ]
                    }"""
        ]

        testList "LogicalTypes" [
            testCase "Decimal" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<decimal>
                    <| Schema.ofString """{"type": "string"}"""
            testCase "GUID" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<System.Guid>
                    <| Schema.ofString """{"type": "string"}"""
            testCase "DateTime" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<System.DateTime>
                    <| Schema.ofString """{"type": "string"}"""
            testCase "DateTimeOffset" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<System.DateTimeOffset>
                    <| Schema.ofString """{"type": "string"}"""
            testCase "TimeSpan" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<System.TimeSpan>
                    <| Schema.ofString """{"type": "string"}"""
            testCase "Uri" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<System.Uri>
                    <| Schema.ofString """{"type": "string"}"""
            testCase "UriInGenericRecord" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<GenericRecord<System.Uri>>
                    <| Schema.ofString """{"type":"record","name":"Foo.Bar.GenericRecord_Of_Uri","fields":[{"name":"Value","type":"string"}]}"""
        ]

        testList "Annotations" [
            testCase "Aliases" <| fun _ ->
                let ann = """{
                    "records": [
                        {"name": "Foo.Bar.NewRecord",
                         "aliases": ["Foo.Bar.OldRecord"],
                         "fields": [
                            {"name": "Caption", "aliases": ["Title", "Cap"]},
                            {"name": "Description", "aliases": [], "default": "Not Yet Described"}
                        ]}
                    ]}"""
                expectSchemasEqual
                    <| generateSchema' ann typeof<NewRecord>
                    <| Schema.ofString """{
                        "type": "record",
                        "name": "Foo.Bar.NewRecord",
                        "aliases": ["OldRecord"],
                        "fields" : [
                            {"name": "Id", "type": "int"},
                            {"name": "Caption", "aliases": ["Title", "Cap"], "type": "string"},
                            {"name": "Description", "type": "string", "default":"Not Yet Described"}
                        ]
                    }"""
            testCase "EnumAliasesAndDefaultValue" <| fun _ ->
                let ann = """{
                    "enums": [
                        {
                         "name": "Foo.Bar.NewTestState",
                         "aliases": ["Foo.Bar.TestState"],
                         "default": "Blue"
                        }
                    ]}"""
                expectSchemasEqual
                    <| generateSchema' ann typeof<NewTestState>
                    <| Schema.ofString """{
                        "type": "enum",
                        "name": "Foo.Bar.NewTestState",
                        "aliases":["Foo.Bar.TestState"],
                        "symbols": ["Yellow", "Red", "Blue"],
                        "default":"Blue"
                    }"""
        ]

        testList "ComplexTypes" [
            testCase "ItemRecord" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<ItemRecord>
                    <| Schema.ofString """{
                        "type":"record",
                        "name":"Foo.Bar.ItemRecord",
                        "fields":[
                            {"name":"Id","type":"int"},
                            {"name":"Name","type":"string"},
                            {"name":"Price","type":[
                                {
                                    "type":"record",
                                    "name":"Foo.Bar.Price.Price",
                                    "fields":[
                                        {"name":"Item","type":"string"}
                                    ]
                                }
                            ]}
                        ]}"""
            testCase "Basket" <| fun _ ->
                expectSchemasEqual
                    <| generateSchema typeof<Basket>
                    <| Schema.ofString """{
                        "type":"array",
                        "items":[
                            {
                                "type":"record",
                                "name":"Foo.Bar.LineItem.SaleItem",
                                "fields":[
                                    {"name":"Item1","type":{
                                        "type":"record",
                                        "namespace": "",
                                        "name":"Tuple_Of_String_And_String_And_Decimal",
                                        "fields":[
                                            {"name":"Item1","type":"string"},
                                            {"name":"Item2","type":"string"},
                                            {"name":"Item3","type":"string"}
                                        ]
                                    }},
                                    {"name":"Item2","type":"string"}
                                ]
                            },
                            {
                                "type":"record",
                                "name":"Foo.Bar.LineItem.TenderItem",
                                "fields":[
                                    {"name":"Item1","type":[
                                        {
                                            "type":"record",
                                            "name":"Foo.Bar.Tender.Cash",
                                            "fields":[]
                                        },
                                        {
                                            "type":"record",
                                            "name":"Foo.Bar.Tender.Card",
                                            "fields":[
                                                {"name":"Item","type":"string"}

                                            ]
                                        },
                                        {
                                            "type":"record",
                                            "name":"Foo.Bar.Tender.Voucher",
                                            "fields":[{"name":"Item","type":"string"}]
                                        }]
                                    },
                                    {"name":"Item2","type": "string"}
                                ]
                            },
                            {
                                "type":"record",
                                "name":"Foo.Bar.LineItem.CancelItem",
                                "fields":[{"name":"Item","type":"int"}]
                            }
                        ]}"""
        ]
    ] |> testList "Schema"