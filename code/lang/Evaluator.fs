module Evaluator

open AST

(*
 * each constructor must do something
 * only time you do a reduction is when there is a Application ( Abstraction (...), ...)       
 * alpha reduction, beta reduction, elim parenthesis 
 *    
*)

// let rec eval (e: Expr) : Expr = 
//     match e with 
//     | Variable v -> e
//     | Abstraction (a,b) -> e
//     | Application (first, second) ->
//         match first with
//         | Abstraction (variable, content) -> 
//             // replace any instance of VARIABLE in CONTENT with SECOND
//             // have to search content recursively and replace all instances of variable with second
//             let rec beta_reduce var con sec = 
//                 match content with
//                 | 
//             beta_reduce variable content second
//         | _ -> eval e



