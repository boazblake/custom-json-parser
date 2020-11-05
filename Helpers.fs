module Helpers
  open System
  open Types

  let printResult result =
    match result with
    | Success (value, input) ->
      printfn "%A" value
    | Failure (label, err, parserPos) ->
      let errLine = parserPos.CurrentLine
      let colPos = parserPos.Column
      let linePos = parserPos.Line
      let failureCaret = sprintf "%*s^%s" colPos "" err
      printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errLine failureCaret
      printfn "Error parsing %s\n%s" label err

  let getLabel parser =
    parser.Label

  let setLabel parser newLabel =
    let parseFn input =
      let result = parser.ParseFn input
      match result with
      | Success s ->
        Success s
      |  Failure (label, err, parserPos) ->
          Failure(newLabel, err, parserPos)

    { ParseFn = parseFn; Label = newLabel }
  let ( <?> ) = setLabel


  let initialPosition = { Line = 0; Column = 0}

//next character
  let incrCol (pos:Position) =
    {pos with Column = pos.Column + 1}

//next line
  let incrLine (pos:Position) =
    {Line = pos.Line + 1; Column  = 0}

// inputState from String
  let fromStr (str: String) =
    if String.IsNullOrEmpty(str) then
      { Lines=[||]; Position = initialPosition}
    else
      let separators = [| "\r\n"; "\n" |]
      let lines = str.Split(separators, StringSplitOptions.None)
      { Lines = lines; Position = initialPosition }


  //Logic for next char:
    // 1) if line >= maxLine ->
    // return EOF
    // 2) if col less than line length ->
    // return char at colPos, increment colPos
    // 3) if col at line length ->
    // return NewLine, increment linePos


  let currentLine inputState =
    let linePos = inputState.Position.Line
    if linePos < inputState.Lines.Length then
      inputState.Lines.[linePos]
    else
      "End of File"

  let nextChar input =
    let linePos = input.Position.Line
    let colPos = input.Position.Column

    if linePos >= input.Lines.Length then
      input, None
    else
      let currentLine = currentLine input
      if colPos < currentLine.Length then
        let char = currentLine.[colPos]
        let newPos = incrCol input.Position
        let newState = {input with Position = newPos}
        newState, Some char
      else
        let char = '\n'
        let newPos = incrLine input.Position
        let newState = {input with Position = newPos}
        newState, Some char


  let rec readAllChars input =
    [
      let remainingInput,charOpt = nextChar input
      match charOpt with
      | None ->
      //end of file
        ()
      | Some nextChar ->
        yield nextChar
        yield! readAllChars remainingInput
    ]


  let parserPositionFromInputState (inputState: InputState) = {
    CurrentLine = currentLine inputState
    Line = inputState.Position.Line
    Column = inputState.Position.Column
  }

  let satisfy predicate label =
    let parseFn input =
      let remainingInput,charOpt = nextChar input
      let pos = parserPositionFromInputState input
      match charOpt with
      | None ->
        let err = "No more input"
        Failure (label, err, pos)
      | Some nextChar ->
        if predicate nextChar then
          Success (nextChar, remainingInput)
        else
          let err = sprintf "Unexpexted %c" nextChar
          Failure (label, err, pos)
    { ParseFn = parseFn; Label=label}


  let run parser input =
    parser.ParseFn input

  let whiteSpaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

  let rec parseZeroOrMore parser input =
    let firstResult = run parser input
    match firstResult with
    | Failure _ -> ([], input)
    | Success (firstValue, inputAfterFirstParse) ->
      let (subsequentValues, remainingInput) =
        parseZeroOrMore parser inputAfterFirstParse
      let values = firstValue::subsequentValues
      (values, remainingInput)

  let many parser =
    let label = "unknown"
    let rec innerFn input =
      Success(parseZeroOrMore parser input)
    { ParseFn = innerFn; Label = label }

