namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Solver.Parser
open Solver.AST
open Solver.Evaluator

[<TestClass>]
type Test1 () =

    [<TestMethod>]
    member this.SimpleParserTest () = 
        let input = "Lx.xy"
        let result = parse input
        match result with
        | Some actual -> 
            let expected = Abstraction ('x', Application (Variable 'x', Variable 'y'))
            let is_same = expected = actual
            Assert.IsTrue is_same
        | None -> Assert.IsTrue false


    [<TestMethod>]
    member this.MediumParserTest () = 
        let input = "Lx.(xLy.(xx))"
        let result = parse input
        match result with
        | Some actual -> 
            let expected = Abstraction ('x', Application (Variable 'x', Abstraction ('y', Application (Variable 'x', Variable 'x'))))
            let is_same = expected = actual
            Assert.IsTrue is_same
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.MediumParserTest2 () = 
        let input = "(Lx.Ly.xyy)(La.a)b"
        let result = parse input
        match result with
        | Some actual -> 
            let expected = 
                Application (
                    Application ( 
                        Abstraction ('x', Abstraction ('y', Application ( Application (Variable 'x', Variable 'y'), Variable 'y'))),
                        Abstraction ('a', Variable 'a')
                    ),
                    Variable 'b'
                )
            let is_same = expected = actual
            Assert.IsTrue is_same
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.ComplexParserTest () = 
        let input = "(Ln.Lf.Lx.f(nfx))(Lf.Lx.x)"
        let result = parse input
        match result with
        | Some actual -> 
            let expected = 
                Application (
                    Abstraction (
                        'n', Abstraction (
                            'f', Abstraction (
                                'x', Application (
                                    Variable 'f',
                                    Application (
                                        Application (
                                            Variable 'n',
                                            Variable 'f'
                                        ),
                                        Variable 'x'
                                    )
                                )))),
                    Abstraction ('f', Abstraction ('x', Variable 'x'))
                )
            let is_same = expected = actual
            Assert.IsTrue is_same
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.SimpleEvaluatorTest () = 
        let log = []
        let input = Application (Abstraction ('x', Abstraction ('x', Variable 'z')), Variable 'y')
        let result = eval input log
        match result with
        | actual_expr, actual_log -> 
            let expected_expr = "Lx.z"
            let is_same_expr = expected_expr = expr_to_string actual_expr

            let expected_log = [("Lx.z", "β-reduction y / x")]
            let is_same_log = expected_log = actual_log
            
            Assert.IsTrue (is_same_expr && is_same_log)

    [<TestMethod>]
    member this.MediumEvaluatorTest () = 
        let log = []
        let input = 
            Application (Abstraction
                ('x', Abstraction (
                    'y', Application (
                        Variable 'x', Variable 'y')
                    )
                ),
            Abstraction ('x', Application (Variable 'x', Variable 'y')
            ))
        let result = eval input log
        match result with
        | actual_expr, actual_log -> 
            let expected_expr = "La.ay"
            let is_same_expr = expected_expr = expr_to_string actual_expr

            let expected_log = List.rev [("xa", "α-reduction a / y"); ("La.Lx.xya", "β-reduction Lx.xy / x");("ay", "β-reduction a / x")]
            let is_same_log = expected_log = actual_log
            
            Assert.IsTrue (is_same_expr && is_same_log)

    [<TestMethod>]
    member this.SimpleSolverTest () = 
        let input = "(Lx.(Ly.xy))y" // great test for variable capture. If variable capture occurs, reduced expression will be incorrectly be Ly.yy
        let result = parse input
        let log = []
        match result with
        | Some actual -> 
            let actual_expr, actual_log= eval actual log

            let expected_expr = "La.ya"
            let is_same_expr = expected_expr = expr_to_string actual_expr

            let expected_log = List.rev [("xa", "α-reduction a / y"); ("La.ya", "β-reduction y / x")]
            let is_same_log = expected_log = actual_log
            
            Assert.IsTrue (is_same_expr && is_same_log)
        | None -> Assert.IsTrue false

    [<TestMethod>]
        member this.SolverTest () = 
            let input = "(Lx.mx)((Lx.Ly.Lz.x)ohn)o"
            let result = parse input
            let log = []
            match result with
            | Some actual -> 
                let actual_expr, actual_log= eval actual log

                let expected_expr = "moo" // Go Ephs !!
                let is_same_expr = expected_expr = expr_to_string actual_expr

                let expected_log = List.rev [("mLx.Ly.Lz.xohn", "β-reduction Lx.Ly.Lz.xohn / x");("Ly.Lz.o", "β-reduction o / x"); ("Lz.o", "β-reduction h / y");("o", "β-reduction n / z")]
                let is_same_log = expected_log = actual_log
                
                Assert.IsTrue (is_same_expr && is_same_log)
            | None -> Assert.IsTrue false
    
    

