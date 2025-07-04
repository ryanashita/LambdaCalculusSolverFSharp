module Solver.AST

type Expr = 
| Variable of char
| Abstraction of char * Expr
| Application of Expr * Expr