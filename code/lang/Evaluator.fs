module Evaluator

open AST

(*
 * The evaluator, or solver, for this lambda calculus implementation.
 * It alpha-reduces starting from a .. z
*)

// picks a new variable that isn't used in the expression already
let pick_new_variable (used : Set<char>) = 
    // returns the first char in the alphabet that isn't in the 'used' set
    let new_variable = List.tryFind (fun x -> not (Set.contains x used)) ['a' .. 'z']
    match new_variable with 
    | Some var -> var
    | None -> failwith "ran out of variables. brown case"

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
let rec beta_reduction (variable : char) (body : Expr) (other) : Expr = 
    match body with 
    | Variable x -> if x = variable then other else Variable x
    | Abstraction (x, body) when x <> variable ->
        // check for free variables here, and if YES, do alpha reduction before beta reduction
        if Set.contains x (free_variables other) then
            let new_variable = pick_new_variable (Set.union (free_variables body) (free_variables other))
            let renamed = alpha_reduction x new_variable body
            Abstraction (new_variable, beta_reduction variable renamed other)
        else 
            Abstraction (x, beta_reduction variable body other)
    | Abstraction (x, body) -> Abstraction (x, body)
    // recurse on both sides of the application
    | Application (a,b) -> 
        Application (beta_reduction variable a other, beta_reduction variable b other)

// evaluates / solves the lambda expression
let rec eval (e: Expr) : Expr = 
    match e with 
    | Variable v -> Variable v
    | Abstraction (variable,body) -> Abstraction (variable, eval body)
    // beta reduce on a reducible expression (redex) : when an abstraction is on the left side in an applicatino
    | Application ( Abstraction (variable, content), arg) ->
        let reduced = beta_reduction variable content arg
        eval reduced
    | Application (a,b) ->
        let a' = eval a
        let b' = eval b
        // in the case a' ends up evaluating to an abstraction, evaluate recursively
        match a' with
        | Abstraction (x, body) -> eval (Application (a', b'))
        | _ -> Application (a', b')



