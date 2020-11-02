module Types

  type Aparser = string  -> (bool * string)

  type Result<'a> =
    | Success of 'a
    | Failure of string

  type Msg = string

  type ParseChar = char -> (string -> Result<char * string>)

  type Parser<'T> = Parser of (string -> Result<'T * string>)
