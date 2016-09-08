module Eval

type Position = FParsec.Position
type Literal = Parser.Literal
type Expr = Parser.Expr

type farray( dims : int list, elms : Value array ) =
    let dimensions         = dims
    let elements           = elms
    member this.Dimensions = dimensions
    member this.Elements   = elms

and Value =
    | Integer of int
    | Float   of float
    | String  of string
    | Array   of farray

[<RequireQualifiedAccessAttribute>]
type Type = Integer | Float | String | Array | Number | Or of Type * Type 

type Error = 
    | NoSuchFunction    of string * Position
    | NotDyadic         of string * Position
    | NotMonadic        of string * Position
    | TypeError         of string * Type * Type
    | LengthError       of string * int list * int list
    | UnshapableError   of string * int list * int list
    | InvalidDimensions of string
    | InvalidState      of string

type Result<'a> =
    | Ok  of 'a
    | Err of Error

// Pass on the error if `l` is an `Err` otherwise return `r x` where `x` is the
// `Ok x` of `l`
let (-->) l r =
    match l with
    | Ok a -> r a
    | Err err -> Err err

// If `r` is `Ok`, apply `f` on the inner value of `Ok`
let fmap f r =
    match r with
    | Ok v -> Ok <| f v
    | Err err -> Err err

module FArray =
    let ofList (l : Value list) =
        new farray( [ l.Length ], Array.ofList l )

    // Apply a function resulting in Result<Value> over each element of the 
    // FArray. If an `Err` is returned that error will be returned instead
    // exiting early. Otherwise the resulting FArray is returned in an `Ok`  
    let doEach f (arr: farray) =
        // Recursively apply `f` to the `idx`th element of `elms` and store it
        // into the index `idx` of `nelms` if it succeeded, otherwise return the
        // error.
        let rec doIt (elms : Value array) (nelms : Value array) idx =
            // Check if we've reached the end of the array
            if idx = elms.Length
            then Ok nelms
            else f elms.[idx] --> fun r ->
                nelms.[idx] <- r; doIt elms nelms (idx + 1)
        // Create the initial empty array with placeholder values
        let initial = Array.create arr.Elements.Length (Integer 0)
        doIt arr.Elements initial 0 --> fun elms ->
            // If `doIt` succeeds, then create a new farray with the same
            // dimensions but with the new elements
            Ok <| new farray( arr.Dimensions, elms )
    
    // Same as `doEach` except it applies the function `f` over `a` and `b`s
    // elements pair-wise. This operation is only valid if the `farray`s have
    // the same dimensions.
    let doEach2 f (a : farray) (b : farray) =
        assert (a.Dimensions = b.Dimensions)

        let rec doIt (aelms : Value array) (belms : Value array) (nelms : Value array) idx =
            if idx = aelms.Length
            then Ok nelms
            else f aelms.[idx] belms.[idx] --> fun r ->
                nelms.[idx] <- r; doIt aelms belms nelms (idx + 1)
        
        let initial = Array.create a.Elements.Length (Integer 0)
        (doIt a.Elements b.Elements initial 0) --> fun elms ->
            Ok <| new farray( a.Dimensions, elms )
    
    let toString (arr : farray) =
        let dims = String.concat "x" <| List.map (string) arr.Dimensions
        let body = String.concat " " <| Array.map (sprintf "%A") arr.Elements
        dims + "\n" + body

module List =
    let rec doEach f =
        function
        | e::es -> f e --> fun r -> fmap (fun x -> r :: x) <| doEach f es
        | []    -> Ok []

let isNumber =
    function
    | Integer _ | Float _ -> true
    | _                   -> false

let typeOf = function
    | Integer _ -> Type.Integer
    | Float   _ -> Type.Float
    | String  _ -> Type.String
    | Array   _ -> Type.Array

let rec isType a =
    function
    | Type.Or (b1, b2) -> isType a b1 || isType a b2
    | b -> a = b

type Context = Map<string, FuncKind>

and MonadicFunc = (Context -> Value -> Result<Value>)
and DyadicFunc = (Context -> Value -> Value -> Result<Value>)

and FuncKind =
    | Monadic of MonadicFunc
    | Dyadic  of DyadicFunc
    | Both    of MonadicFunc * DyadicFunc

let literalToValue =
    function
    | Literal.Integer i -> Integer i
    | Literal.Float   f -> Float f
    | Literal.String  s -> String s

let runExpr ctx expr =
    let rec callMonadic f (args : Parser.PExpr list) =
        (eval args.[0]) --> (fun earg -> f ctx earg)
    and callDyadic f (args : Parser.PExpr list) =
        eval args.[0] --> fun arg0 ->
            eval args.[1] --> fun arg1 ->
                f ctx arg0 arg1
    and eval =
        function
        // Literals evalute to their runtime representation
        | (Expr.Lit l, _)  -> literalToValue l |> Ok
        // Arrays evalute to a runtime array where all it's values have been
        // evalutated.
        | (Expr.Arr ls, _) -> List.doEach eval ls --> (Ok << Array << FArray.ofList)
        // A function call evaluates to the result of the function applied to
        // it's evalutaed arguments
        | (Expr.FnC {Name = fn; Arguments = args}, pos) ->
            match (Map.tryFind fn ctx, List.length args) with
            | (Some (Monadic f), 1)   -> callMonadic f args
            | (Some (Dyadic f), 2)    -> callDyadic f args
            | (Some (Both (m, d)), x) -> if x = 1 then callMonadic m args
                                                  else callDyadic d args
            | (Some _, 1) -> Err <| NotMonadic (fn, pos)
            | (Some _, 2) -> Err <| NotDyadic (fn, pos)
            | (None, _) -> NoSuchFunction (fn, pos) |> Err
            | (_, i) -> Err <| InvalidState (sprintf "Invalid arg count %i" i)
    eval expr