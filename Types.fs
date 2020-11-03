module Types
  type ParserLabel = string
  type ParserErr = string

  type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserErr

  type ParseChar = char -> (string -> Result<char * string>)

  type Parser<'a> = {
    ParseFn : (string -> Result<'a * string>)
    Label : ParserLabel
  }
