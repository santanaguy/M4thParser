module Cenas

open Swensen.Unquote
open NUnit.Framework
open fsharpLearn.mainModule
open fsharpLearn.Parser
open fsharpLearn.Types
open System

let plusT = OpToken(Plus)
let plusNotParsed = Operator(Plus)
let (+) exp1 exp2 = Operation(exp1, Plus, exp2)
let minusT = OpToken(Minus)
let minusNotParsed = Operator(Minus)
let (-) exp1 exp2 = Operation(exp1, Minus, exp2)
let timesT = OpToken(Multiply)
let timesNotParsed = Operator(Multiply)
let (*) exp1 exp2 = Operation(exp1, Multiply, exp2)
let dividedByT = OpToken(Divide)
let dividedByNotParsed = Operator(Divide)
let (/) exp1 exp2 = Operation(exp1, Divide, exp2)
let (^) exp1 exp2 = Operation(exp1, Power, exp2)
let oneT = NumberToken(1m, "1")
let endGroupT = GroupEndToken
let startGroupT = GroupStartToken
let zero = Number(0m)
let one = Number(1m)
let two = Number(2m)
let xVariableT = LetterToken("x")
let xVariable = Variable("x")
let yVariableT = LetterToken("y")
let yVariable = Variable("y")
let power b p= Operation(b, Power, p) 
let sqrtT = FunctionToken("sqrt")
let parseComplete = getTokens >> buildExpressions >> inferMultiplications >> inferMissingZeroes >> inferOperations

[<Test>]
let ``Parses simple expressions correctly``() = 
    let output = getTokens "1+1" |> buildExpressions
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
    let output = getTokens "1+-1--1" |> buildExpressions |> inferMissingZeroes
    test <@output = [one; plusNotParsed; zero; minusNotParsed; one; minusNotParsed; zero; minusNotParsed; one]@>

[<Test>]
let ``Detects implicit multiplication``()= 
    let output = getTokens "1x1(1)" |> buildExpressions |> inferMultiplications
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
    let output = getTokens "11(1)1x1x(1)x" |> buildExpressions |> inferMultiplications 
    test <@output = [Number(11m); timesNotParsed; Group[one]; timesNotParsed; 
                        one; timesNotParsed; xVariable; 
                        timesNotParsed; one; timesNotParsed; 
                        xVariable; timesNotParsed; Group[one]; timesNotParsed; xVariable]@>

[<Test>]
let ``InferMultiplications infers correctly inside groups``()=
    let output = getTokens "(1(xy)(x)1)" |> buildExpressions |> inferMultiplications 
    test <@output = [Group[one; timesNotParsed; Group[xVariable; timesNotParsed; yVariable;]; timesNotParsed; Group[xVariable]; timesNotParsed; one;]]@>

[<Test>]
let ``InferMultiplications infers correctly functions``()=
    let output = getTokens "1sqrt(1)1sqrt(1)" |> buildExpressions |> inferMultiplications 
    test <@output = [one; timesNotParsed; Sqrt(Param[one]); timesNotParsed; one; timesNotParsed; Sqrt(Param[one]);]@>

[<Test>]
let ``InferMultiplications does not infer when function args``()=
    let output = getTokens "sqrt(1)" |> buildExpressions |> inferMultiplications 
    test <@output = [Sqrt(Param[one]);]@>

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
let ``inferOperations parses correctly simple``() =
    let output = parseInput "1+1" |> inferOperations 
    test <@output = [one + one]@>

[<Test>]
let ``inferOperations parses correctly chain``() =
    let output = parseInput "1+1+1" |> inferOperations 
    test <@output = [(one + one) + (one)]@>

[<Test>]
let ``inferOperations parses correctly precedence simple``() =
    let output = parseInput "1+0*1+2" |> inferOperations
    test <@output = [((one) + (zero * one)) + two]@>

[<Test>]
let ``inferOperations respects order when priority is the same``() =
    let output = parseInput "1+0-1+2" |> inferOperations
    test <@output = [((one + zero) - (one)) + two]@>

[<Test>]
let ``inferOperations respects priority``() =
    let output = parseInput "1+0*1/2-0" |> inferOperations 
    test <@output = [(one + ((zero * one) / two)) - zero]@>

[<Test>]
let ``inferOperations respects groups simple``() =
    let output = parseInput "1(1+0)" |> inferOperations
    test <@output = [one * (Group [one + zero])]@>

    let output = parseInput "(1+2)-2" |> inferOperations
    test <@output =  [Group[one + two] - two]@>

[<Test>]
let ``parseOperators respects groups nested``() =
    let output = parseInput "1((1+x) * (0+y))" |> inferOperations
    test <@output = [one * Group[Group[one + xVariable] * Group[zero + yVariable]]]@>

[<Test>]
let ``inferOperations parses exponents``() =
    let output = parseInput "2^2" |> inferOperations
    test <@output = [two ^ two]@>

[<Test>]
let ``inferOperations parses exponents on groups``() =
    let output = parseInput "2^(2+1)" |> inferOperations
    test <@output = [two ^ Group[two + one]]@>

[<Test>]
let ``parseFunctions parses correct function``() =
    let output = [LetterToken("s");LetterToken("q");LetterToken("r");LetterToken("t");startGroupT;oneT;endGroupT] |> parseFunctions
    test <@output = [sqrtT; startGroupT; oneT; endGroupT]@>

[<Test>]
let ``buildExpressions adds arguments to sqrt``() = 
    let outpupt = getTokens "1+sqrt(1x)sqrt(1)" |> buildExpressions 
    test <@outpupt = [one; plusNotParsed; Sqrt(Param[one; xVariable]); Sqrt(Param([one]))]@>

[<Test>]
let ``buildExpressions recognizes sin``() =
    let output = getTokens "sin(1)" |> buildExpressions
    test <@ output=[Sin(Param([one]))] @>

[<Test>]
let ``sin is recognized correctly``() = 
    let output = parseComplete "sin(1+1)"
    test <@output = [Sin(Param([one + one]))]@>

[<Test>]
let ``sin is recognized correctly when in larger expression``() = 
    test <@parseComplete "1 + sin(1 + 1)" = [one + Sin(Param[one + one])]@>
    test <@parseComplete "sin(1 + 1) + 1" = [Sin(Param[one + one]) + one] @>
    test <@parseComplete "1 + sin(1 + 1) + 1" = [(one + Sin(Param[one + one])) + one]@>

[<Test>]
let ``cos is recognized correctly``() = 
    let output = parseComplete "cos(1+1)"
    test <@output = [Cos(Param([one + one]))]@>

[<Test>]
let ``cos is recognized correctly when in larger expression``() = 
    test <@parseComplete "1 + cos(1 + 1)" = [one + Cos(Param[one + one])]@>
    test <@parseComplete "cos(1 + 1) + 1" = [Cos(Param[one + one]) + one] @>
    test <@parseComplete "1 + cos(1 + 1) + 1" = [(one + Cos(Param[one + one])) + one]@>

[<Test>]
let ``tan is recognized correctly``() = 
    let output = parseComplete "tan(1+1)"
    test <@output = [Tan(Param([one + one]))]@>

[<Test>]
let ``tan is recognized correctly when in larger expression``() = 
    test <@parseComplete "1 + tan(1 + 1)" = [one + Tan(Param[one + one])]@>
    test <@parseComplete "tan(1 + 1) + 1" = [Tan(Param[one + one]) + one] @>
    test <@parseComplete "1 + tan(1 + 1) + 1" = [(one + Tan(Param[one + one])) + one]@>