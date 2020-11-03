module Helpers
  open System
  open Types

  let printResult result =
    match result with
    | Success (value, input) ->
      printfn "%A" value
    | Failure (label, err) ->
      printfn "Error parsing %s\n%s" label err

  let getLabel parser =
    parser.Label

  let setLabel parser newLabel =
    let parseFn input =
      let result = parser.ParseFn input
      match result with
      | Success s ->
        Success s
      | Failure (_, err) ->
          Failure(newLabel, err)

    { ParseFn = parseFn; Label = newLabel }
  let ( <?> ) = setLabel

