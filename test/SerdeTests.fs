module SerdeTests

open System.Linq
open System.Collections.Generic

open Fable.Mocha
open Fable.Avro
open Foo.Bar
open Fable.SimpleJson


type Comparer = string -> obj -> obj -> unit
type SimpleCase = {Name: string; Instance: obj; InstanceType: System.Type; Comparer: Comparer }
type SimpleCaseList = {Name: string; Cases: SimpleCase list}

let inline simpleCase'<'T> comparer name (instance:'T) =
    {Name = name; Comparer = comparer; Instance = instance; InstanceType = typeof<'T>}

let inline simpleCase<'T when 'T:equality> name (instance:'T) =
    {Name = name; Comparer = (fun msg v1 v2 -> Expect.equal (unbox v1) (unbox v2) msg); Instance = instance; InstanceType = typeof<'T>}

let simpleCaseList name cases = {Name = name; Cases = cases}

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
    ]
]

let jsonSimpleTest (case:SimpleCase) =
    testCase case.Name <| fun _ ->

        let serializer = JsonSerde.createSerializer' case.InstanceType []
        let json = serializer case.Instance

        printfn "Serialization result: %s" <| SimpleJson.toString json

        let deserializer = JsonSerde.createDeserializer' case.InstanceType []
        let copy = deserializer json

        case.Comparer "Copy should be equal to original" case.Instance copy

let serdeTests =
    [
        simpleCases
        |> List.map (fun list -> testList list.Name (list.Cases |> List.map jsonSimpleTest))
        |> testList "Json"

    ] |> testList "Serde"