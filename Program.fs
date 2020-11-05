// Learn more about F# at http://fsharp.org

open System
open Types
open Helpers
open Parsers
open Model

[<EntryPoint>]
let main argv =

     // let parseAB =
     //      (anyOf ['a'..'z'])  .>>. (anyOf ['0'..'9'])
     //      <?> "lettersAndNumbers"
     run spaces (fromStr "123")
     |> printResult
     0 // return an integer exit code
