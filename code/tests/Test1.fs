namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST

[<TestClass>]
type Test1 () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);

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

