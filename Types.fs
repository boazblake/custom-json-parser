module Types
  type ParserLabel = string
  type ParserErr = string
  type ParserPosition = {
    CurrentLine : string
    Line : int
    Column : int
  }

  type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserErr * ParserPosition

  type ParseChar = char -> (string -> Result<char * string>)

  type Position = {
    Line: int
    Column: int
  }

  type InputState  = {
    Lines : string[]
    Position : Position
   }


  type Input = InputState
  type Parser<'a> = {
    ParseFn : (Input -> Result<'a * Input>)
    Label : ParserLabel
  }

