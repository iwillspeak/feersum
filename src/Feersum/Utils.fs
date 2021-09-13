module Utils

module ResultEx =

    /// Unwrap a result into the inner value. Useful for testing, or in cases
    /// where other inference indciates the result will be `Ok`.
    let unwrap = function
        | Ok x -> x
        | Error e -> failwithf "Called unwrap on `Error`: %A" e
    
    /// Check if the result is OK
    let isOk = function
        | Ok _ -> true
        | Error _ -> false

    /// Check if the result is OK
    let isError = function
        | Ok _ -> false
        | Error _ -> true

    /// Collect a list of results into a single result. If all results are `Ok`
    /// then `Ok` is returned with the inner values as a list. If any result is
    /// `Error` the first such is returned.
    let collect input =
        let rec decompose = function
            | [] -> ([],None)
            | Result.Error e::_ -> ([],Some(e))
            | Result.Ok v::rest ->
                let (results,err) = decompose rest
                (v::results,err)
        let (results, maybeErr) = decompose input
        match maybeErr with
        | Some e -> Result.Error e
        | None -> Result.Ok results    

    /// Extract the value from a result, or fallback to a default value.
    let okOr fallback = function
        | Ok o -> o
        | Error _ -> fallback

module OptionEx =

    /// Unwrap an option into the inner value. Useful for testing, or in cases
    /// where other inference indicates the result will be `Some`.
    let unwrap = function
        | Some s -> s
        | None -> failwith "Called unwrap on a `None`."

    /// Turn a result with a unit error into an option
    let ofResult = function
        | Result.Ok o -> Some(o)
        | Error () -> None
