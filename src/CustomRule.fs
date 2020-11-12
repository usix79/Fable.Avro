namespace Fable.Avro

open System
open Fable.SimpleJson

type CustomRule = {
    InstanceType: System.Type;
    SurrogateType: System.Type
    SerializationCast: obj -> obj
    DeserializationCast: obj -> obj
    StubValue: Json
}

module CustomRule =
    let buidInRules = [
        {
            InstanceType = typeof<Uri>
            SurrogateType = typeof<string>
            SerializationCast = fun v -> v.ToString() |> unbox
            DeserializationCast = fun v -> Uri(unbox(v)) |> unbox
            StubValue =  JString ""
        }
    ]