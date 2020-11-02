module Parsers

  open Types


  let Aparser (str:string) =
    printfn "str: %s" str
    if str.Length = 0 then
      (false,"")
    else if str.[0] = 'A' then
      let st = str.[1..]
      (true, st)
    else
      (false,str)


  let parseStringWithChar char (str: string) =
      if str.Length = 0 then
        Failure "End of input"
      else
        let first = str.[0]
        if first = char then
          let remaining = str.[1..]
          Success (char, remaining)
        else
          let first = str.[0]
          Failure (sprintf "No Character %c found in the string %s, found %c" char str first)


  let ParseChar char =
    Parser (parseStringWithChar char)
