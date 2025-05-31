open Parser

[<EntryPoint>]
let main argv = 
    let input = argv[0]
    printfn "%s" input
    
    let ast_maybe = parse input

    match ast_maybe with
    | Some ast -> printfn "%A" ast
    | None -> printfn "Invalid Program"
    0
