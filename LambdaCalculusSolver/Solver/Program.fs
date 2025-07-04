open Solver.Parser
open Solver.AST
open Solver.Evaluator
open Solver.ToCSharp

(* 
 * The evaluator, or solver, for this lambda calculus implementation.
 * It alpha-reduces starting from a .. z
 * It beta-reduces in the normal order
 *)

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
    let log = []

    let ast_maybe = parse input
    match ast_maybe with
    | Some ast -> 
        let reduced, steps = eval ast log
        let reduction_steps = List.rev steps
        printfn ""
        prettyprint reduction_steps
        printfn "Reduced Expression - %A" (expr_to_string reduced)
        printfn "%s" (toCSharp reduced)

    | None -> printfn "Invalid Program"
    0
