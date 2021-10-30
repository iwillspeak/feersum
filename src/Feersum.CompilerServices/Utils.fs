namespace Feersum.CompilerServices.Utils

module Result =

    /// Unwrap a `Result` into the inner value. Useful for testing, or in cases
    /// where other inference indciates the result will be `Ok`.
    let unwrap =
        function
        | Ok x -> x
        | Error e -> failwithf "Called unwrap on `Error`: %A" e

    /// Check if the result is OK
    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    /// Check if the result is OK
    let isError =
        function
        | Ok _ -> false
        | Error _ -> true

    /// Collect a list of results into a result of lists. If all values are `Ok`
    /// then an `OK` is returned containing the results. If any errors are
    /// encoutnered then an `Error` is returned containing all the erorrs.
    let collectAll input =
        let rec decompose =
            function
            | [] -> ([], [])
            | head :: rest ->
                let (results, errs) = decompose rest
                match head with
                | Result.Ok o -> (o :: results, errs)
                | Result.Error e -> (results, e :: errs)

        let (results, errors) = decompose input

        match errors with
        | [] -> Result.Ok results
        | errors -> Result.Error errors
    
    /// Collect a list of results into a single result. If all results are `Ok`
    /// then `Ok` is returned with the inner values as a list. If any result is
    /// `Error` the first such is returned.
    let collect results =
        results
        |> collectAll
        |> Result.mapError (List.head)

    /// Extract the value from a result, or fallback to a default value.
    let okOr fallback =
        function
        | Ok o -> o
        | Error _ -> fallback

module Option =

    /// Unwrap an option into the inner value. Useful for testing, or in cases
    /// where other inference indicates the result will be `Some`.
    let unwrap =
        function
        | Some s -> s
        | None -> failwith "Called unwrap on a `None`."

    /// Turn a result with a unit error into an option
    let ofResult =
        function
        | Result.Ok o -> Some(o)
        | Error () -> None
