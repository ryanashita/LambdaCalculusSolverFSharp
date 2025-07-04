module Solver.ToCSharp

open Solver.AST

(* 
 * Converts a lambda calculus expression to a C# representation.
 * The output is a string that can be used in C# code.
 *)

let rec toCSharp (expr: Expr) : string = 
    match expr with
    | Variable x -> x.ToString()
    | Abstraction (x, body) -> 
        let csharpBody = toCSharp body
        $@"((Func<dynamic, dynamic>)((dynamic {x}) => {csharpBody}))"
    | Application (left, right) -> 
        let csharpLeft = toCSharp left
        let csharpRight = toCSharp right
        $"({csharpLeft})({csharpRight})"