module Builtins

open Eval

let unzip f (a, b) = f a b

let rec elementWise fn f a b =
    let g = (elementWise fn f)
    match (a, b) with
    | (Array aa, Array ab) ->
        if aa.Dimensions <> ab.Dimensions
        then Err <| LengthError (fn, aa.Dimensions, ab.Dimensions) 
        else (FArray.doEach2 g aa ab) --> (Ok << Array)
    | (Array aa, _) -> (FArray.doEach (fun x -> g x b) aa) --> (Ok << Array)
    | (_, Array ab) -> (FArray.doEach (g a) ab) --> (Ok << Array)
    | _ -> f a b

let numFunc fn fi ff a b =
    match (a, b) with
    | (Float fa, Float fb)          -> Ok <| Float (ff fa fb)
    | (Float fa, Integer ib)        -> Ok <| Float (ff fa <| float ib)
    | (Integer ia, Float fb)        -> Ok <| Float (ff (float ia) fb)
    | (Integer ia, Integer ib)      -> Ok <| Integer (fi ia ib)
    | (Integer _, _) | (Float _, _) -> Err <| TypeError (fn, typeOf b, Type.Number)
    | (_, Integer _) | (_, Float _) -> Err <| TypeError (fn, typeOf a, Type.Number)
    | _ -> Err <| InvalidState fn

let iota ctx count =
    match count with
    | Integer i -> (Ok << Array << FArray.ofList) <| List.map (Integer) [ 0 .. i - 1 ]
    | v -> Err <| TypeError ("iota", typeOf v, Type.Integer)

let add ctx a b = numFunc "+" (+) (+) a b
let sub ctx a b = numFunc "-" (-) (-) a b
let mul ctx a b = numFunc "*" (*) (*) a b
let div ctx a b = numFunc "/" (/) (/) a b
let rec inv ctx =
    function
    | Integer i -> Ok << Float <| 1.0 / (float i)
    | Float f   -> Ok << Float <| 1.0 / f
    | Array vs  -> FArray.doEach (inv ctx) vs --> (Ok << Array)
    | v -> Err <| TypeError ("/", typeOf v, Type.Or (Type.Float, Type.Integer ))

let rec neg ctx =
    function
    | Integer i -> Ok <| Integer -i
    | Float f   -> Ok <| Float -f
    | Array aa  -> FArray.doEach (neg ctx) aa --> (Ok << Array)
    | v -> Err <| TypeError ("-", typeOf v, Type.Number)

// Keep the values of `vals` but change the dimensions to `dims`
// Valid transformation:
// * If `vals` is only a single value, then it will get 
//   replicated across each element of the new array with the dimensions `dims`
// * If the amount of elements in `vals` is equal to the amount of elements in
//   it's new dimensions
// * TODO: If the amount of elements in `vals` is a multiple of the amount of 
//   elements in the new dimensions `dims`
let reshape ctx dims vals =
    let reshapeIt adims =
        let count = List.fold (*) 1 adims 
        match vals with
        | Array elms -> if elms.Elements.Length <> count
                        then if count % elms.Elements.Length = 0
                             then Ok <| new farray( adims,
                                   [| for i in 0 .. count - 1 ->
                                      elms.Elements.[ i % elms.Elements.Length ] |] )
                             else Err <| UnshapableError ("reshape", elms.Dimensions, adims)
                        else Ok <| new farray( adims, elms.Elements )
        | Integer v  -> Ok <| new farray( adims, [| for _ in 0 .. count - 1 -> Integer v |] )
        | _ -> Err <| TypeError ("reshape", typeOf vals, Type.Or (Type.Array, Type.Integer))

    let asInteger =
        function
        | Integer v -> Ok v
        | _         -> Err <| InvalidDimensions "reshape"
    
    match dims with
    | Array adims ->
        List.doEach asInteger (List.ofArray adims.Elements) --> fun ldims ->
            fmap Array <| reshapeIt ldims
    | Integer d   -> fmap Array <| reshapeIt [d]
    | _           ->
        Err <| TypeError ("reshape", typeOf dims, Type.Or (Type.Array, Type.Integer))

    
let shape ctx =
    function
    | Array arr -> (Ok << Array) <| new farray( [arr.Dimensions.Length],
                     (Array.ofList << (List.map Integer)) arr.Dimensions )
    | _ -> Ok <| Integer 1

// TODO: mod, sum-prod, length, rank
// and, or, not, eq, les, gre, leq, geq

let stdContext = 
    [ "iota"   , Monadic iota 
    ; "+"      , Dyadic ((elementWise "+") << add)
    ; "-"      , Both (neg, ((elementWise "-") << sub))
    ; "*"      , Dyadic ((elementWise "-") << mul)
    ; "/"      , Both (inv, ((elementWise "-") << div))
    ; "reshape", Dyadic reshape
    ; "shape"  , Monadic shape ]
    |> Map.ofList
