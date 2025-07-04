module Solver.Parser

open Solver.Combinator
open Solver.AST

(*
 *         <expr> ::= <variables>
 *                 |  <abstraction>
 *                 |  <application>
 *                 |  <parens>
 *         <parens> ::= ( <expr> )
 *  <application> ::= <expr><expr>
 *  <abstraction> ::= L<variable>.<expr>
 *    <variables> ::= a | .. | z    
*)

// define recursive parser pexpr
let pexpr,pexprImpl = recparser()

let pvar = pletter |>> Variable <!> "variable"

let pabstraction = pright (pchar 'L') (pseq (pleft pletter (pchar '.')) pexpr Abstraction) <!> "abstraction"

let pparens = pbetween (pchar '(') pexpr (pchar ')') <!> "parens"

// parser for the different types of constructors, sans application. To work around left-recursion
let ptype = pabstraction <|> pparens <|> pvar <!> "type"

// working around left recursion by calling ptype, and using fold to create one Application Expr. 
let papplication = pseq ptype (pmany1 ptype) (fun (a,b) -> List.fold (fun acc arg -> Application(acc,arg)) a b) <|> pseq pabstraction pexpr Application <!> "application"

// exprs can be applications here
pexprImpl := pabstraction <|> papplication <|> pparens <|> pvar <!> "expr"

let grammar = pleft pexpr peof <!> "grammmar"

let parse (input: string) = 
    let i = prepare input
    match grammar i with
    | Success (ast,_) -> Some ast
    | Failure (_,_) -> None



