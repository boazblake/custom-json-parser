module Parsers
  open System
  open Types
  open Helpers

  let Aparser (str:string) =
    printfn "str: %s" str
    if str.Length = 0 then
      (false,"")
    else if str.[0] = 'A' then
      let st = str.[1..]
      (true, st)
    else
      (false,str)

  let satisfy predicate label =
    let parseFn input =
      if String.IsNullOrEmpty(input) then
        Failure (label, "No More input")
      else
        let first = input.[0]
        if predicate first then
          let remaining = input.[1..]
          Success (first, remaining)
        else
          let err = sprintf "Unexpexted %c" first
          Failure (label, err)
    {ParseFn = parseFn; Label=label}

  let ParseChar char =
    let predicate ch = (ch = char)
    let label = sprintf "%c" char
    satisfy predicate label
