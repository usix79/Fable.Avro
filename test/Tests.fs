module Tests

open Fable.Mocha

let dummyTest =
    testCase "Dummy Test" <| fun _ ->
        Expect.equal true true "They are equal"

[<EntryPoint>]
let main args = Mocha.runTests dummyTest