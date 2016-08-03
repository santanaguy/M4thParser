module Cenas

open NUnit.Framework
open fsharpLearn.mainModule
open fsharpLearn.Parser
open fsharpLearn.Types
open FsUnit
open System

let plusT = OperatorToken(Plus,"+")
let plusNotParsed = NotYetParsed(plusT)
let plus exp1 exp2 = Operator(exp1, Plus, exp2)
let minusT = OperatorToken(Minus,"-")
let minusNotParsed = NotYetParsed(minusT)
let timesT = OperatorToken(Multiply,"*")
let timesNotParsed = NotYetParsed(timesT)
let times exp1 exp2 = Operator(exp1, Multiply, exp2)
let dividedByT = OperatorToken(Divide,"/")
let dividedByNotParsed = NotYetParsed(dividedByT)
let oneT = NumberToken(1m, "1")
let endGroupT = GroupEndToken(")")
let startGroupT = GroupStartToken("(")
let zero = Number(0m)
let one = Number(1m)
let two = Number(2m)
let xVariableT = VariableToken("x", "x")
let xVariable = Variable("x")
let yVariableT = VariableToken("y", "y")
let yVariable = Variable("y")

[<Test>]
let ``Parses simple expressions correctly``() = 
    let test = parseInput "1+1"
    test |> should equal [one; plusNotParsed; one]

[<Test>]
let ``Parses simple group correctly``()=
    let test = parseInput "(1-1)"
    test |> should equal [Group([one; minusNotParsed; one])]

[<Test>]
let ``Parses nested groups correctly``()=
    let test = parseInput "((1+1)+((1+1)-1))"
    test |> should equal [Group[Group[one; plusNotParsed; one]; plusNotParsed; Group[Group([one; plusNotParsed; one]); minusNotParsed; one]]]

[<Test>]
let ``Parses nested groups correctly2``()=
    let test = parseInput "((1)+1)"
    test |> should equal [Group[Group([one]); plusNotParsed; one]]

[<Test>]
let ``Parses nested groups correctly3``()=
    let test = parseInput "((1-1)-(1+1))"
    test |> should equal [Group[Group[one; minusNotParsed; one]; minusNotParsed; Group[one; plusNotParsed; one;]]]

[<Test>]
let ``Detects implicit zero``()= 
    let test = parseInput "1+-1--1"
    test |> should equal [one; plusNotParsed; zero; minusNotParsed; one; minusNotParsed; zero; minusNotParsed; one]

[<Test>]
let ``Detects implicit multiplication``()= 
    let test = parseInput "1x1(1)"
    test |> should equal [one; timesNotParsed; xVariable; timesNotParsed; one; timesNotParsed; Group([one])]

[<Test>]
let ``getTokensFromGroup deals with inner group correctly 2``()=
    let test = getTokens "(1+1)+1)+1" |> getTokensFromGroup 
    test |> should equal [startGroupT; oneT; plusT; oneT; endGroupT; plusT; oneT; endGroupT;]

[<Test>]
let ``getTokensFromGroup stops adding after group closed``()=
    let test = getTokens "1+1)+(1)" |> getTokensFromGroup 
    test |> should equal [oneT; plusT; oneT; endGroupT;]

[<Test>]
let ``InferMultiplications infers correctly simple``()=
    let test = getTokens "11(1)1x1x(1)x" |> inferMultiplications
    test |> should equal [NumberToken(11m, "11"); timesT; startGroupT; oneT; endGroupT; 
                        timesT; oneT; timesT; xVariableT; timesT; oneT; timesT; xVariableT; 
                        timesT; startGroupT; oneT; endGroupT; timesT; xVariableT]

[<Test>]
let ``InferMultiplications infers correctly inside groups``()=
    let test = getTokens "(1(xy)(x)1)" |> inferMultiplications
    test |> should equal [startGroupT; oneT; timesT; startGroupT; xVariableT; timesT; yVariableT; endGroupT; timesT; startGroupT; xVariableT; endGroupT; timesT; oneT; endGroupT]

[<Test>]
let ``parseOperators parses correctly simple``() =
    let test = parseInput "1+1" |> parseOperators Plus
    test |> should equal [plus one one]

[<Test>]
let ``parseOperators parses correctly chain``() =
    let test = parseInput "1+1+1" |> parseOperators Plus
    test |> should equal [plus (plus one one) (one)]

[<Test>]
let ``parseOperators parses correctly precence simple``() =
    let test = parseInput "1+0*1+2" |> parseOperators Multiply |> parseOperators Plus
    test |> should equal [plus (plus (one) (times zero one)) two]