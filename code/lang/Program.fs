open Parser
open AST
open Evaluator

// let rec prettyprint (input: list<string*string>) count =
//     match input with
//     | x::rest -> 
//         printfn "Step %d: %s - %s" count (fst x) (snd x)
//         let new_count = count + 1
//         prettyprint rest new_count
//     | [] -> ()

let prettyprint (input: list<string*string>) =
    ignore (
        input 
        |> List.fold (fun count (expr, desc) -> 
            printfn "Step %d: %s | %s" count expr desc
            count + 1
        ) 1
    )
    
        

[<EntryPoint>]
let main argv = 
    let input = argv[0]
    // printfn "%s" input

    let log = []

    let ast_maybe = parse input

    match ast_maybe with
    | Some ast -> 
        // printfn "AST representation - %A" ast
        let reduced, steps = eval ast log
        let reduction_steps = List.rev steps
        printfn ""
        prettyprint reduction_steps
        printfn "Reduced Expression - %A" (expr_to_string reduced)
    | None -> printfn "Invalid Program"
    0

    // let is = (Variable 'x', "this is a variable") :: log
    // printfn "%A" is
