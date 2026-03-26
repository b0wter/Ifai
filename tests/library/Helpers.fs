namespace Ifai.Library.Tests.Helpers

module Result =
    let forceOk (r: Result<'a, _>) : 'a =
        match r with
        | Ok a -> a
        | _ -> failwith "Could not force Ok value out of Error result"

    let forceError (r: Result<_, 'e>) : 'e =
        match r with
        | Error e -> e
        | _ -> failwith "Could not force Error value out of Ok result"