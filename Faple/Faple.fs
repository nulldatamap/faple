module Faple

open FParsec

let parseString src =
    runParserOnString Parser.expression () "<stdin>" src

// Prints a mutli-dimensionala matrix of atoms (doesn't work with nested 
// matricies yet).
let printMatrix (m : Eval.farray) =
    // Recusively join together each element, either with a space if it's a row
    // otherwise with a newline (Newlines accumilate across greater dimensions)
    let rec printRank rank (elms : string list) : string =
        match rank with
        | []  -> invalidOp "Invalid null-rank farray"
        // It's a row:
        | [_] -> String.concat " " elms
        | s::e::es ->
            List.map (printRank (e::es)) (List.chunkBySize (elms.Length / s) elms) |> String.concat "\n"
    
    // Turn all the elements into strings and remember what the widest string was
    let stringifyWithBiggest biggest arr =
        Array.foldBack (fun v (max, elms) ->
            let sv = sprintf "%A" v
            if sv.Length > max
            then (sv.Length, sv::elms)
            else (max, sv::elms)) arr (1, [])
    
    // Go through and left-pad all strings with spaces so they have the same width
    let fitLength len (e : string) =
        if e.Length < len
        then (String.replicate (len - e.Length) " ") + e
        else e
    
    let maxLen, elms = stringifyWithBiggest 1 m.Elements
    List.map (fitLength maxLen) elms |> printRank m.Dimensions |> printfn "%s"

let printResult =
    function
    | Eval.Ok (Eval.Value.Array elms) -> printMatrix elms
    | Eval.Ok r                       -> printfn "%A" r
    | Eval.Err err                    -> printfn "Error: %A" err

let rec repl ctx =
    printf "> "
    let srcLine = System.Console.ReadLine()
    match parseString srcLine with
    | Success (r, _, _) -> printResult <| Eval.runExpr ctx r
                           repl ctx
    | Failure (errmsg, _, _) -> printf "%s" errmsg

[<EntryPoint>]
let main argv =
    repl Builtins.stdContext
    0
