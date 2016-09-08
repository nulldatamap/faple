module Faple

open FParsec

let parseString src =
    runParserOnString Parser.expression () "<stdin>" src

let printResult =
    function
    | Eval.Ok (Eval.Value.Array elms) -> printfn "> %s" <| Eval.FArray.toString elms
    | Eval.Ok r                       -> printfn "> %A" r
    | Eval.Err err                    -> printfn "Error: %A" err

let rec repl ctx =
    let srcLine = System.Console.ReadLine()
    match parseString srcLine with
    | Success (r, _, _) -> printResult <| Eval.runExpr ctx r
                           repl ctx
    | Failure (errmsg, _, _) -> printf "%s" errmsg

[<EntryPoint>]
let main argv =
    repl Builtins.stdContext
    0 // return an integer exit code
