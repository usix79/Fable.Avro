module SerdeTests

open System.Collections.Generic

open Fable.Mocha
open Fable.Avro
open Foo.Bar


type Comparer = string -> obj -> obj -> unit
type SimpleCase = {Name: string; Instance: obj; InstanceType: System.Type; Comparer: Comparer }
type SimpleCaseList = {Name: string; Cases: SimpleCase list}
type EvolutionCase = {Name: string; Instance: obj; InstanceType: System.Type; ExpectedInstance: obj; ExpectedType: System.Type; Annotations:string; Comparer: Comparer }

let inline simpleCase'<'T> comparer name (instance:'T) =
    {Name = name; Comparer = comparer; Instance = instance; InstanceType = typeof<'T>}

let inline simpleCase<'T when 'T:equality> name (instance:'T) =
    {Name = name; Comparer = (fun msg v1 v2 -> Expect.equal v1 v2 msg); Instance = instance; InstanceType = typeof<'T>}

let simpleCaseList name cases = {Name = name; Cases = cases}

let inline evolutionCase<'TSource, 'TDest when 'TDest:equality> name (instance:'TSource) (expectedInstance:'TDest) annotations =
    {Name = name; Comparer = (fun msg v1 v2 -> Expect.equal v1 v2 msg);
        Instance = instance; InstanceType = typeof<'TSource>;
        ExpectedInstance = expectedInstance; ExpectedType = typeof<'TDest>; Annotations = annotations}

let inline compareSequences<'T when 'T:equality> msg (expected:obj) (actual:obj) =
    let expected = expected :?> seq<'T> |> List.ofSeq
    let actual = actual :?> seq<'T> |> List.ofSeq
    Expect.equal actual expected msg

let inline compareSets<'T when 'T:equality and 'T : comparison> msg (expected:obj) (actual:obj) =
    let expected = expected :?> seq<'T> |> List.ofSeq |> List.sort
    let actual = actual :?> seq<'T> |> List.ofSeq |> List.sort
    Expect.equal actual expected msg

let inline compareDictionaries<'TValue> msg (expected:obj) (actual:obj) =
    let expected = expected :?> seq<KeyValuePair<string,'TValue>> |> List.ofSeq |> List.sortBy (fun pair -> pair.Key)
    let actual = actual :?> seq<KeyValuePair<string,'TValue>> |> List.ofSeq |> List.sortBy (fun pair -> pair.Key)
    Expect.equal actual expected msg

let inline compareAsStrings msg (expected:obj) (actual:obj) =
    Expect.equal (sprintf "%A" actual) (sprintf "%A" expected) msg

let simpleCases = [
    simpleCaseList "Primitive" [
        simpleCase "string" "Hello World!!!"
        simpleCase "empty string" ""
        simpleCase "bool true" true
        simpleCase "bool false" false
        simpleCase "int" 125
        simpleCase "float" 543.
        simpleCase "long" 789L
        simpleCase "float32" 101.2f
    ]
    simpleCaseList "Enum" [
        simpleCase "SimpleEnum" TestState.Yellow
    ]
    simpleCaseList "Array" [
        simpleCase "bytes" [|00uy; 255uy; 12uy; 16uy; 00uy|]
        simpleCase "List" ["One"; "Two"; "Three"]
        simpleCase "Array" [|"One"; "Two"; "Three"|]
        simpleCase "Set" (["One"; "Two"; "Three"] |> Set.ofList)
        simpleCase "Collection" (List(["One"; "Two"; "Three"]))
        simpleCase' compareSets "HashSet" (["One"; "Two"; "Three"] |> HashSet)
        simpleCase' compareSequences "Seq" (seq {"One"; "Two"; "Three"})
        simpleCase "RecordWithArray" {Value = ["Name1"; "Name2"; "Name3"]}
    ]
    simpleCaseList "Map" [
        let pairs = ["One", 1; "Two", 2; "Three", 3]

        simpleCase "Map" (pairs |> Map.ofList)

        let dict = Dictionary<string,int>()
        pairs |> Seq.iter dict.Add
        simpleCase' compareDictionaries<int> "Dictionary" dict

        simpleCase "RecordWithMap" {Value = pairs |> Map.ofList}
    ]
    simpleCaseList "Record" [
        simpleCase "SimpleRecord" {Id = 1; Name = "Hello World!!!"; Version = 2L}
        simpleCase "ParentRecord" {
                Chield1 =  {Id = 1; Name = "Hello World!!!"; Version = 2L}
                Chield2 =  {Id = 3; Name = "World, Hello!!!"; Version = 1L} }
    ]
    simpleCaseList "Tuple" [
        simpleCase "Tuple" (123, "Hello")
        simpleCase "Tuple Record" {Value = (123, "Hello")}
    ]
    simpleCaseList "Union" [
        simpleCase "Result Ok" ((Ok "Hello"):Result<string,string>)
        simpleCase "Result Error" ((Error "Hello"):Result<string,string>)
        simpleCase "BinaryTree" (Node (Leaf "XXX", Node (Leaf "YYY", Leaf "ZZZ")))
    ]
    simpleCaseList "Nullable" [
        simpleCase "Option-Some" (Some "Hello World!!!")
        simpleCase "Option-None" (None:Option<string>)
        simpleCase "Nullable in Record" {Value = Some "Hello World!!!"}
    ]
    simpleCaseList "LogicalTypes" [
        simpleCase "Decimal" 3.1415926m
        simpleCase "Scaled Decimal" {Id = 124; Caption = ""; Price = 199.99m}
        simpleCase "ItemRecord" {Id = 123; Name ="Item"; Price = Price 9.99m}
    ]
    simpleCaseList "ComplexTests" [
        let basket:Basket = [
            SaleItem ((*Product*)("XXX-1", "Product 1", 10m<GBP/Q>), 1m<Q>)
            SaleItem ((*Product*)("XXX-2", "Product 2", 1m<GBP/Q>), 10m<Q>)
            SaleItem ((*Product*)("XXX-3", "Product 3", 3.14m<GBP/Q>), 5m<Q>)
            TenderItem (Cash, 100m<GBP>)
            TenderItem ((Card "1111-1111-1111-1111"), 15m<GBP>)
            CancelItem 2
        ]
        simpleCase "Basket" basket
    ]
    simpleCaseList "CustomTypes" [
        simpleCase "Single Guid" (System.Guid.NewGuid())
        simpleCase "Guid" {Value = System.Guid.NewGuid()}
        simpleCase' compareAsStrings "Uri" {Value = System.Uri("http://www.example.com")}
        simpleCase "DateTime" {Value = System.DateTime.UtcNow}
        simpleCase "DateTimeOffset" {Value = System.DateTimeOffset.UtcNow}
        simpleCase "TimeSpan" {Value = System.TimeSpan.FromSeconds(321.5)}
        simpleCase "BigInteger" {Value = System.Numerics.BigInteger.Parse("12134324239474828818828747110108719417219247192477444444224242424244242")}
    ]
]

let evolutionCases = [
    """{
        "records": [
            {
                "name": "Foo.Bar.RecordWithNewField",
                "aliases": ["Foo.Bar.Foo.Bar.RecordWithId"],
                "fields": [
                    {"name": "NewField", "default": "Hello"}
                ]
            }
        ]
    }"""
    |> evolutionCase "Added string field" ({Id=456}:RecordWithId) ({Id=456; NewField="Hello" }:RecordWithNewField)

    """{
        "records": [
            {
                "name": "Foo.Bar.NewRecord",
                "aliases": ["Foo.Bar.OldRecord"],
                "fields": [
                    {"name": "Caption", "aliases": ["Title", "Cap"]},
                    {"name": "Description", "aliases": [], "default": "Not Yet Described"}
                ]
            }
        ]
    }"""
    |>evolutionCase "New Record" ({Id=456; Title="Hello World!!!"}:OldRecord) ({Id=456; Caption="Hello World!!!"; Description="Not Yet Described"}:NewRecord)

    """{
        "enums": [
            { "name": "Foo.Bar.NewTestState", "aliases": ["Foo.Bar.TestState"], "default": "Blue" }
    ]}"""
    |> evolutionCase "New Enum" (TestState.Green) (NewTestState.Blue)

    """{
        "records": [
            { "name": "Foo.Bar.RecordV2", "aliases": ["Foo.Bar.RecordV1"]},
            { "name": "Foo.Bar.UnionV2.Case3", "aliases": ["Foo.Bar.UnionV1.Case1"]}
        ]
    }"""
    |> evolutionCase "Rename of Union Case" ({Union = Case1 "Hello!"}:RecordV1) ({Union = Case3 "Hello!"}:RecordV2)

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
    |> evolutionCase "UnknownUnion in Record" ({Union = Case3 "Hello!"}:RecordV2) ({Union = UnionV1.UnknownCase}:RecordV1)
]

let jsonSimpleTest (case:SimpleCase) =
    testCase case.Name <| fun _ ->
        let serializer = JsonSerde.createSerializer' case.InstanceType []
        let json = serializer case.Instance
        printfn "Serialization result: %s" <| Fable.SimpleJson.SimpleJson.toString json
        let deserializer = JsonSerde.createDeserializer' case.InstanceType [] ""
        let copy = deserializer json
        case.Comparer "Copy should be equal to original" copy case.Instance

let jsonEvolutionTest (case:EvolutionCase) =
    testCase case.Name <| fun _ ->
        let serializer = JsonSerde.createSerializer' case.InstanceType []
        let json = serializer case.Instance
        //printfn "Serialization result: %s" <| SimpleJson.toString json
        let deserializer = JsonSerde.createDeserializer' case.ExpectedType [] case.Annotations
        let copy = deserializer json
        case.Comparer "Deserialized value should be equal to expected" copy case.ExpectedInstance

let serdeTests =
    [
        simpleCases
        |> List.map (fun list -> testList list.Name (list.Cases |> List.map jsonSimpleTest))
        |> testList "Json"

        evolutionCases
        |> List.map jsonEvolutionTest
        |> testList "Json Evolution"

    ] |> testList "Serde"