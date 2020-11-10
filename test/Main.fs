module Tests

open Fable.Mocha


[<EntryPoint>]
let main args =
    testList "All" [SerdeTests.serdeTests; SchemaTests.schemaTests]
    |> Mocha.runTests