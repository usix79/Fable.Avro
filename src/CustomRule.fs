namespace Fable.Avro

open System
open System.Globalization

type CustomRule = {
    InstanceType: System.Type;
    SurrogateType: System.Type
    SerializationCast: obj -> obj
    DeserializationCast: obj -> obj
}

module CustomRule =
    let buidInRules = [
        {
            InstanceType = typeof<Uri>
            SurrogateType = typeof<string>
            SerializationCast = fun v -> v.ToString() |> unbox
            DeserializationCast = fun v -> Uri(unbox(v)) |> unbox
        }
    ]