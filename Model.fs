module Model

  open Types
  open Helpers
  open Parsers

  let bindP fn firstParser =
    let label = "unknown"
    let innerFn input =
      let result1 = run firstParser input
      match result1 with
      | Failure (label, err, pos) -> Failure (label, err, pos)
      | Success (value1, remainingInput) ->
        let nextParser = fn value1
        run nextParser remainingInput
    {ParseFn = innerFn; Label=label}
    //infix
  let (>>=) parser fn = bindP fn parser

  let returnP x =
    let label = "unknown"
    let innerFn input =
      Success(x, input)
    { ParseFn = innerFn; Label = label }

  let mapP fn =
    bindP (fn >> returnP)
  let (<!>) = mapP
  let (|>>) x f = mapP f x


  let applyP fP xP =
    fP >>= ( fun f ->
    xP >>= (fun x ->
      returnP (f x)))

  let ( <*> ) = applyP


  let lift2 f xP yP =
    returnP f <*> xP <*> yP


  let andThen parser1 parser2 =
    let label = sprintf "%s andThen %s" (getLabel parser1) (getLabel parser2)
    parser1 >>= (fun result1 ->
    parser2 >>= (fun result2 ->
      returnP (result1, result2)))
  let (.>>.) = andThen

  let orElse parser1 parser2 =
    let label = "unknown"
    let innerFn input =
      let result1 = run parser1 input

      match result1 with
      | Success result ->
          result1
      | Failure (label, err, pos) ->
        let result2 = run parser2 input
        result2

    {ParseFn = innerFn; Label = label }
  let (<|>) = orElse

  let choice listOfParsers =
    List.reduce (<|>)  listOfParsers

  let anyOf listOfChars =
    let label = sprintf "any of %A" listOfChars
    listOfChars
      |> List.map parseChar
      |> choice
      <?> label

  let mapP' fn parser =
    let label = "unknown"
    let innerFn input =
      let result = run parser input

      match result with
      | Success (value, remaining) ->
        let newVal = fn value
        Success (newVal, remaining)
      | Failure (label, err, pos) ->
        Failure (label, err, pos)

    {ParseFn = innerFn; Label = label}

  let applyP' fP xP =
    (fP .>>. xP)
      |> mapP' (fun (f,x) -> f x)

  let addP =
    lift2 (+)

  let startsWith (str: string) (prefix: char) =
    str.StartsWith(prefix)

  let startsWithP =
    lift2 startsWith

  let rec sequence parserList =
    let cons head tail = head::tail

    let consP = lift2 cons

    match parserList with
    | [] ->
      returnP []
    | head::tail ->
      consP head (sequence tail)


  let charListToString charList =
    System.String(List.toArray charList)


  let many1 parser =
    parser >>= (fun head ->
    many parser >>= fun tail ->
      returnP (head::tail))

  let spaces = many whiteSpaceChar
  let spaces1 = many1 whiteSpaceChar

  let many1' parser  =
    let label = "unknown"
    let rec innerFn input =
      let firstResult = run parser input

      match firstResult with
      | Failure (label, err, pos) ->
        Failure (label, err, pos)
      | Success (firstValue, inputAfterFirstParse) ->
        let (subseqValue, remainingInput) = parseZeroOrMore parser inputAfterFirstParse

        let values = firstValue :: subseqValue
        Success(values, remainingInput)

    {ParseFn = innerFn; Label = label }

  let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none


  let (.>>) p1 p2 =
    p1 .>>. p2
    |> mapP' (fun (a,b) -> a)

  let (>>.) p1 p2 =
    p1 .>>. p2
    |> mapP' (fun (a,b) -> b)

  let between p1 p2 p3 =
    p1 >>. p2 .>> p3


  let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p, pList) -> p::pList

  let sepBy p sep =
    sepBy1 p sep <|> returnP []
