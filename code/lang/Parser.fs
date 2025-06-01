module Parser

open Combinator
open AST

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

// parser for the Variable constructor
let pvar = pletter |>> Variable <!> "variable"

// parser for the Abstraction constructor
let pabstraction = pright (pchar 'L') (pseq (pleft pletter (pchar '.')) pexpr Abstraction) <!> "abstraction"

// parser for parens
let pparens = pbetween (pchar '(') pexpr (pchar ')') <!> "parens"

// parser for the different types of constructors
let ptype = pabstraction <|> pparens <|> pvar <!> "type"

// parser for the Application Parser
let papplication = pseq ptype (pmany1 ptype) (fun (a,b) -> List.fold (fun acc arg -> Application(acc,arg)) a b) <|> pseq pabstraction pexpr Application <!> "application"

// define pexpr
pexprImpl := pabstraction <|> papplication <|> pparens <|> pvar <!> "expr"

// define the grammar
let grammar = pleft pexpr peof <!> "grammmar"

// parse function to convert into an AST
let parse (input: string) = 
    let i = debug input
    match grammar i with
    | Success (ast,_) -> Some ast
    | Failure (_,_) -> None



