open Parser
open AST
open Evaluator

[<EntryPoint>]
let main argv = 
    let input = argv[0]
    printfn "%s" input
    
    let ast_maybe = parse input

    match ast_maybe with
    | Some ast -> 
        printfn "AST representation - %A" ast
        let reduced = eval ast
        printfn "Reduced - %A" reduced
    | None -> printfn "Invalid Program"
    0
