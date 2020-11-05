module Parsers
  open System
  open Helpers

  let parseChar char =
    let predicate ch = (ch = char)
    let label = sprintf "%c" char
    satisfy predicate label

  let whiteSpaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

  let digitChar =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

// let Aparser (str:string) =
  //   printfn "str: %s" str
  //   if str.Length = 0 then
  //     (false,"")
  //   else if str.[0] = 'A' then
  //     let st = str.[1..]
  //     (true, st)
  //   else
  //     (false,str)
