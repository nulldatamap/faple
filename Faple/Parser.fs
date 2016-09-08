module Parser

open FParsec

[<RequireQualifiedAccessAttribute>]
type Literal = 
    | Integer of int32
    | Float of float
    | String of string

[<RequireQualifiedAccessAttribute>]
type Expr =
    | Lit of Literal
    | Arr of list<PExpr>
    | FnC of FunctionCall

and PExpr = Expr * Position

and FunctionCall =
    { Name      : string
    ; Arguments : list<PExpr> }

let annotatePos pos pe =
    pe |>> fun e -> (e, pos)

// Reads a valid indentifier
let pident<'u> : Parser<string, 'u> =
    let identInit = letter <|> pchar '_'
    let identBody = identInit <|> digit
    (many1Chars2 identInit identBody)
    <|> (many1Satisfy <| isAnyOf "+-*%^/.,<>=!@#$&|?")
    <?> "valid identifier or symbol"

// Reads a given parser followed by whitespace
let ws p =
    p .>> unicodeSpaces

// Create reference to the expression parser, since it's recursive
let expression, expressionRef = createParserForwardedToRef()

let stringLiteral =
    // Replace the character after the escape onset with it's value
    let escapeSequence = anyOf "\\\"nr"
                        |>> function
                            | '\\' -> "\\"
                            | 'n'  -> "\n"
                            | 'r'  -> "\r"
                            | '"'  -> "\""
                            | c    -> string c 
    let strChar = manySatisfy <| isNoneOf "\r\n\""
    let strEscChar = (pchar '\\') >>. escapeSequence
    // Read string chars and escape sequences and concat them
    between (pchar '"') (pchar '"') <| stringsSepBy strChar strEscChar

// Number settings for both reading integers, floats and hex numbers:
let numberSettings = NumberLiteralOptions.AllowExponent
                  ||| NumberLiteralOptions.AllowFraction
                  ||| NumberLiteralOptions.AllowMinusSign
                  ||| NumberLiteralOptions.AllowHexadecimal

let intOrFloat =
    numberLiteral numberSettings "number"
        |>> fun num ->
            if num.IsInteger
            then Literal.Integer (int32 num.String)
            else Literal.Float (float num.String)

let literal =
    intOrFloat
    <|> (stringLiteral |>> Literal.String)
    <?> "a literal"

// Which is currently either a literal or a sub expression
// variables and the like would be here too in the future
let rec item =
    getPosition >>= fun pos ->
      annotatePos pos (literal |>> Expr.Lit)
      <|> (between (pchar '(') (pchar ')') <| expression)
      <?> "value"

// Try reading one of more items, if there's more than one make it an array 
let maybeArray =
    getPosition >>= fun pos ->
      (many1 (ws item) >>=
          (fun x -> match x with
                    | [l] -> preturn l
                    | ls  -> annotatePos pos (preturn <| Expr.Arr ls)))
    <?> "value or array"

// Try parsing a function call that only takes one arguments ( to the right )
let monadic : Parser<PExpr, 'u> =
    getPosition >>= fun pos ->
        ((tuple2 (ws pident) (ws expression)) >>=
            fun (fname, rightarg) ->
                annotatePos pos <| (Expr.FnC { Name = fname; Arguments = [rightarg] } |> preturn))
    <?> "monodic call"

// Try parsing a function call that takes two arguments (one on each side)
// or if there's only values but no function name, then just parse the values
let dyadic : Parser<PExpr, 'u> =
    let createDyadic l pos (n, r) =
        annotatePos pos <| (Expr.FnC { Name = n; Arguments = [l; r] } |> preturn)
    
    getPosition >>= fun pos ->
        (maybeArray >>= fun left ->
            ((tuple2 (ws pident) (ws expression)) >>= (createDyadic left pos))
            <|> preturn left)
    <?> "dyadic call"

do expressionRef := monadic <|> dyadic <|> maybeArray

// Reads a list of expressions
let program : Parser<list<PExpr>, unit> =
    ws <| (many <| ws expression) .>> eof

