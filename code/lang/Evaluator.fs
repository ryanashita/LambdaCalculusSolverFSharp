module Evaluator

open AST

(*
 * The evaluator, or solver, for this lambda calculus implementation.
 * It alpha-reduces starting from a .. z
 * It beta-reduces in the normal order
*)

// log to keep track of all reduction steps
type Log = (string * string) list

let rec expr_to_string (input: Expr) : string = 
    match input with 
    | Variable x -> string x
    | Abstraction (x, body) -> "L" + string x + "." + expr_to_string body
    | Application (a,b) -> expr_to_string a + expr_to_string b

let pick_new_variable (used : Set<char>) = 
    // returns the first char in the alphabet that isn't in the 'used' set
    let new_variable = List.tryFind (fun x -> not (Set.contains x used)) ['a' .. 'z']
    match new_variable with 
    | Some var -> var
    | None -> failwith "ran out of variables. brown case failed"

// returns a set of all free variables in the lambda expression
let rec free_variables (expr : Expr) = 
    match expr with
    | Variable x -> Set.singleton x
    // remove x from the set of free variables in the body of the abstraction 
    | Abstraction (x, body) -> Set.remove x (free_variables body)
    | Application (a,b) -> Set.union (free_variables a) (free_variables b)

// alpha reduces an abstraction in an expression
let rec alpha_reduction (old_variable : char) (new_variable : char) (body : Expr) : Expr = 
    match body with
    | Variable x -> if x = old_variable then Variable new_variable else Variable x
    // return an abstraction with the body a-reduced if the bound variable is the same as the outer variable. prevents variable capture
    | Abstraction (x, body) when x = old_variable ->
        Abstraction (new_variable, alpha_reduction old_variable new_variable body)
    // no variable capture
    | Abstraction (x, body) ->
        Abstraction (x, alpha_reduction old_variable new_variable body)
    | Application (a,b) ->
        Application (alpha_reduction old_variable new_variable a, alpha_reduction old_variable new_variable b)

// beta reduces an application in an expression
let rec beta_reduction (variable : char) (body : Expr) (other) (log: Log) : Expr * Log = 
    match body with 
    | Variable x -> if x = variable then other, log else Variable x, log
    | Abstraction (x, body) when x <> variable ->
        // check for free variables here, and if YES, do alpha reduction before beta reduction
        if Set.contains x (free_variables other) then
            let new_variable = pick_new_variable (Set.union (free_variables body) (free_variables other))
            let renamed = alpha_reduction x new_variable body
            let alpha_step= "α-reduction " + string new_variable + " / " + string x
            let reduced, new_log = beta_reduction variable renamed other ((expr_to_string renamed, alpha_step) :: log)
            Abstraction (new_variable,reduced), new_log
        else 
            let reduced, new_log = beta_reduction variable body other log
            Abstraction (x, reduced), new_log
    | Abstraction (x, body) -> Abstraction (x, body), log
    // recurse on both sides of the application
    | Application (a,b) -> 
        let a', log1 = beta_reduction variable a other log
        let b', log2 = beta_reduction variable b other log1
        Application (a', b'), log2

let rec eval (e: Expr)(log: Log) : Expr * Log = 
    match e with 
    | Variable v -> Variable v, log
    | Abstraction (variable,body) -> 
        let body', log' = eval body log
        Abstraction (variable, body'), log'
    // beta reduce on a reducible expression (redex) : when an abstraction is on the left side in application
    | Application ( Abstraction (variable, content), arg) ->
        // printfn "Redex - %A" (expr_to_string e)
        let reduced, alpha_log = beta_reduction variable content arg log
        let beta_step = "β-reduction " + expr_to_string arg + " / " + string variable
        let new_log = (expr_to_string reduced, beta_step) :: alpha_log
        eval reduced new_log
    | Application (a,b) ->
        let a', log1 = eval a log
        let b', log2 = eval b log1
        // in the case a' ends up evaluating to an abstraction, evaluate recursively
        match a' with
        | Abstraction (x, body) -> eval (Application (a', b')) log2
        | _ -> Application (a', b'), log2