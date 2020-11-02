// Learn more about F# at http://fsharp.org

open System
open Types
open Parsers
open Model

[<EntryPoint>]
let main argv =
     // let parseA = ParseChar 'A'
    // let parseB = ParseChar 'B'
    // let parseC = ParseChar 'C'
    // let parseBOrParseC = parseB <|> parseC
    // let parseAThenBorC = parseA .>>. parseBOrParseC
    // let parseLowerCase = anyOf ['a'..'z']
    // run parseLowerCase "aBcD"
    // run parse3Digits "123A"
    // run parse3DigitsAsInt "123A"

    // let parsers = [parseA; parseB; parseC]
    // let combinedP = sequence parsers

    // run combinedP "ABCD"

    // let parseABC = ParseString "ABC"


    // run parseABC "AxBCDE"
    // let manyA = many (ParseChar 'A')
    // let manyAB = many (ParseString "AB")

    // run manyAB "ABABCD"
    // run manyA "AACD"
    // run manyA "AAAD"
    // run manyA "!BCD"

    // let digit = anyOf ['1'..'9']

    // let digits = many1 digit

    // run digits "1ABC"
    // run digits "12ABC"
    // run ParseInt "1ABC"
    // run ParseIntSign "-1ABC"
    // run ParseIntSign "8ABC"

    // let digits = many digit

    // let digitSemiColon =
    //     digits .>>. opt (ParseChar ';')

    // run digitSemiColon "1;2;2;"


    // let whitespaceChar = anyOf [' '; '\t'; '\n' ]
    // let whitespace = many1 whitespaceChar

    // let ab = ParseString "AB"
    // let cd = ParseString "CD"
    // let abThenCd = (ab .>> whitespace) .>>. cd

    // run abThenCd "AB \t\nCD"

    // let pdoublequote = ParseChar '"'
    // let quotedInteger = between pdoublequote ParseInt pdoublequote
    // run quotedInteger "\"1234\""
    // run quotedInteger "1234"

    let comma = ParseChar ','
    let digit = anyOf ['0'..'9']

    let zeroOrMoreDigitList = sepBy digit comma
    let oneOrMoreDigitList = sepBy1 digit comma
    run oneOrMoreDigitList "1;"
    // run oneOrMoreDigitList "1,2;"
    // run oneOrMoreDigitList "1,2,3;"
    // run oneOrMoreDigitList "Z;"
    // run zeroOrMoreDigitList "1;"
    // run zeroOrMoreDigitList "1,2;"
    // run zeroOrMoreDigitList "1,2,3;"
    // run zeroOrMoreDigitList "Z;"
    |> printfn "result: %A"
    0 // return an integer exit code
