namespace Fable.Avro

open System
open Fable.SimpleJson

type CustomRule = {
    InstanceType: System.Type;
    SurrogateType: System.Type
    CastToSurrogate: obj -> obj
    CastFromSurrogate: obj -> obj
    StubValue: Json
}

module CustomRule =
    let buidInRules = [
        {
            InstanceType = typeof<Uri>
            SurrogateType = typeof<string>
            CastToSurrogate = fun v -> v.ToString() |> unbox
            CastFromSurrogate = fun v -> Uri(unbox(v)) |> unbox
            StubValue =  JString ""
        }
    ]