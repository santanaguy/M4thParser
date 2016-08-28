﻿module Cenas

open Swensen.Unquote
open NUnit.Framework
open fsharpLearn.mainModule
open fsharpLearn.Parser
open fsharpLearn.Types
open System

let plusT = OpToken(Plus,"+")
let plusNotParsed = Operator(Plus)
let plus exp1 exp2 = Operation(exp1, Plus, exp2)
let minusT = OpToken(Minus,"-")
let minusNotParsed = Operator(Minus)
let minus exp1 exp2 = Operation(exp1, Minus, exp2)
let timesT = OpToken(Multiply,"*")
let timesNotParsed = Operator(Multiply)
let times exp1 exp2 = Operation(exp1, Multiply, exp2)
let dividedByT = OpToken(Divide,"/")
let dividedByNotParsed = Operator(Divide)
let divide exp1 exp2 = Operation(exp1, Divide, exp2)
let oneT = NumberToken(1m, "1")
let endGroupT = GroupEndToken(")")
let startGroupT = GroupStartToken("(")
let zero = Number(0m)
let one = Number(1m)
let two = Number(2m)
let xVariableT = LetterToken("x")
let xVariable = Variable("x")
let yVariableT = LetterToken("y")
let yVariable = Variable("y")
let power b p= Operation(b, Power, p) 
let sqrtT = FunctionToken("sqrt")

[<Test>]
let ``Parses simple expressions correctly``() = 
    let output = parseInput "1+1"
    test <@output = [one; plusNotParsed; one]@>

[<Test>]
let ``Parses simple group correctly``()=
    let output = parseInput "(1-1)"
    test <@output = [Group([one; minusNotParsed; one])]@>

[<Test>]
let ``Parses nested groups correctly``()=
    let output = parseInput "((1+1)+((1+1)-1))"
    test <@output = [Group[Group[one; plusNotParsed; one]; plusNotParsed; Group[Group([one; plusNotParsed; one]); minusNotParsed; one]]]@>

[<Test>]
let ``Parses nested groups correctly2``()=
    let output = parseInput "((1)+1)"
    test <@output = [Group[Group([one]); plusNotParsed; one]]@>

[<Test>]
let ``Parses nested groups correctly3``()=
    let output = parseInput "((1-1)-(1+1))"
    test <@output = [Group[Group[one; minusNotParsed; one]; minusNotParsed; Group[one; plusNotParsed; one;]]]@>

[<Test>]
let ``Detects implicit zero``()= 
    let output = parseInput "1+-1--1"
    test <@output = [one; plusNotParsed; zero; minusNotParsed; one; minusNotParsed; zero; minusNotParsed; one]@>

[<Test>]
let ``Detects implicit multiplication``()= 
    let output = parseInput "1x1(1)"
    test <@output = [one; timesNotParsed; xVariable; timesNotParsed; one; timesNotParsed; Group([one])]@>

[<Test>]
let ``getTokensFromGroup deals with inner group correctly 2``()=
    let output = getTokens "(1+1)+1)+1" |> getTokensFromGroup
    test <@output = [startGroupT; oneT; plusT; oneT; endGroupT; plusT; oneT; endGroupT;]@>

[<Test>]
let ``getTokensFromGroup stops adding after group closed``()=
    let output = getTokens "1+1)+(1)" |> getTokensFromGroup  
    test <@output = [oneT; plusT; oneT; endGroupT;]@>

[<Test>]
let ``InferMultiplications infers correctly simple``()=
    let output = getTokens "11(1)1x1x(1)x" |> inferMultiplications 
    test <@output = [NumberToken(11m, "11"); timesT; startGroupT; 
                        oneT; endGroupT; timesT; 
                        oneT; timesT; xVariableT; 
                        timesT; oneT; timesT; 
                        xVariableT; timesT; startGroupT; 
                        oneT; endGroupT; timesT; xVariableT]@>

[<Test>]
let ``InferMultiplications infers correctly inside groups``()=
    let output = getTokens "(1(xy)(x)1)" |> inferMultiplications
    test <@output = [startGroupT; oneT; timesT; startGroupT; xVariableT; timesT; yVariableT; endGroupT; timesT; startGroupT; xVariableT; endGroupT; timesT; oneT; endGroupT]@>

[<Test>]
let ``InferMultiplications does not infer when function args``()=
    let output = getTokens "sqrt(1)" |> inferMultiplications
    test <@output = [sqrtT; startGroupT; oneT; endGroupT]@>

[<Test>]
let ``joinNumbers joins correctly simple``()=
    let output = [oneT; oneT; plusT; oneT; oneT; ] |> joinNumbers
    test <@output = [NumberToken(11m, "11"); plusT; NumberToken(11m,"11")]@>

[<Test>]
let ``joinDecimals joins decimals correctly``()=
    let output = [oneT; oneT; oneT; DecimalSeparatorToken; oneT; plusT; oneT; oneT; ] |> joinNumbers |> joinDecimals
    test <@output = [NumberToken(111.1m, "111.1"); plusT; NumberToken(11m,"11")]@>

[<Test>]
let ``joinDecimals joins decimals multiple figures``()=
    let output = [oneT; oneT; oneT; DecimalSeparatorToken; oneT; oneT; plusT; oneT; ] |> joinNumbers |> joinDecimals
    test <@output = [NumberToken(111.11m, "111.11"); plusT; NumberToken(1m,"1")]@>

[<Test>]
let ``parseOperators parses correctly simple``() =
    let output = parseInput "1+1" |> parseOperators 
    test <@output = (plus one one)@>

[<Test>]
let ``parseOperators parses correctly chain``() =
    let output = parseInput "1+1+1" |> parseOperators 
    test <@output = (plus (plus one one) (one))@>

[<Test>]
let ``parseOperators parses correctly precedence simple``() =
    let output = parseInput "1+0*1+2" |> parseOperators
    test <@output = (plus (plus (one) (times zero one)) two)@>

[<Test>]
let ``parseOperators respects order when priority is the same``() =
    let output = parseInput "1+0-1+2" |> parseOperators
    test <@output = (plus (minus (plus one zero) (one)) two)@>

[<Test>]
let ``parseOperators respects priority``() =
    let output = parseInput "1+0*1/2-0" |> parseOperators 
    test <@output = (minus (plus one (divide (times zero one) two)) zero)@>

[<Test>]
let ``parseOperators respects groups simple``() =
    let output = parseInput "1(1+0)" |> parseOperators
    test <@output = (times one (Group [plus one zero]))@>

    let output = parseInput "(1+2)-2" |> parseOperators
    test <@output =  (minus (Group[plus one two]) two)@>

[<Test>]
let ``parseOperators respects groups nested``() =
    let output = parseInput "1((1+x) * (0+y))" |> parseOperators
    test <@output = (times one (Group[(times (Group[plus one xVariable]) (Group[plus zero yVariable]))]))@>

[<Test>]
let ``parseOperators parses exponents``() =
    let output = parseInput "2^2" |> parseOperators
    test <@output = (power two two)@>

[<Test>]
let ``parseOperators parses exponents on groups``() =
    let output = parseInput "2^(2+1)" |> parseOperators
    test <@output = (power two (Group[plus two one]))@>

[<Test>]
let ``parseFunctions parses correct function``() =
    let output = [LetterToken("s");LetterToken("q");LetterToken("r");LetterToken("t");startGroupT;oneT;endGroupT] |> parseFunctions
    test <@output = [sqrtT; startGroupT; oneT; endGroupT]@>


