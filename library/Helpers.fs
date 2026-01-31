module Ifai.Library.Helpers

let tryCatch (action: unit -> 'a) : Result<'a, exn> =
    try
        () |> action |> Ok
    with
    | exn -> Error exn