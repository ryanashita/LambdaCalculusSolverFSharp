# Lambda Calculus Solver written in F#
This is a lambda calculus solver written in F#, complete with a parser, evaluator, and full test suite built using MSTest. 

## How to run the solver ##
1. clone this repository on local machine
2. navigate to ```code/lang```
3. type command ```dotnet build``` and enter
4. type command ```dotnet build "--YOUR LAMBDA EXPRESSION HERE--"```

## Output
Running the program with a given input expression will result in a few lines of output. The first few lines (depending on the expression) will be the completed β-reductions and α-reductions. The last line of output is the final reduced expression, preceded by "Reduced Expression -".

## Abstract Syntax Tree (AST) and Backus-Naur form (BNF) ##
The AST is:

```
type Expr =
| Variable of char
| Abstraction of char * Expr
| Application of Expr * Expr
```
where a type Expr has combinators for the three different 'symbols' in a Lambda Calculus AST. Abstractions are the same as lambdas, and Applications are simply two expressions. 

The BNF for this Lambda Calculus grammar is:
```
 *         <expr> ::= <variables>
 *                 |  <abstraction>
 *                 |  <application>
 *                 |  <parens>
 *         <parens> ::= ( <expr> )
 *  <application> ::= <expr><expr>
 *  <abstraction> ::= L<variable>.<expr>
 *    <variables> ::= a | .. | z    
```

## Parser ##
The parser is very simple, with custom made parser combinators for abstractions, variables, parentheses, and applications. The definition for the expression parser, ```pexpr``` is recursive and is fully defined through ```pexprImpl```

All parser combinators in this implementation have debug tags.

## Evaluator ##
The evaluator is a bit more complex, and I will leave the bulk of explaining to the comments in my code and the code itself. At its core, the recursive ```eval``` function recursively solves the lambda expression using pattern matches to manage all cases. The function ```beta_reduction``` does the beta-reducing, and ```alpha_reduction``` solves the alpha-reductions.

One interesting and complicated element of the implementation is deciding when to complete an alpha-reduction. The solver performs an alpha-reduction if a beta-reduction is necessary, but there are free variables in the expression that may be captured (variable capture) if a beta-reduction is ignorantly performed. In the case free variables exist, the solver picks a new unused variable and performs an alpha-reduction. 

## Testing ##
This implementation uses the MSTest framework to test the parser, evaluator, and whole the operation. There are multiple tests for each element of the solver listed. To run the tests: 
1. navigate to the ```code``` directory
2. type ```dotnet build```
3. type ```dotnet test```





