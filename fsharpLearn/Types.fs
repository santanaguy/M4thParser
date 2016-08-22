﻿module fsharpLearn.Types

type OpType = 
    | Plus
    | Minus
    | Multiply
    | Divide
    | Power

type Token = 
    | NumberToken of value : decimal * token : string
    | VariableToken of value : string * token : string
    | OpToken of value : OpType * token : string
    | GroupStartToken of token : string
    | GroupEndToken of token : string
    | DecimalSeparatorToken
    | UnrecognizedToken of token : string
    member x.getExpression() = 
        match x with
        | NumberToken(_, t) | VariableToken(_, t) | OpToken(_, t) | GroupStartToken(t) | GroupEndToken(t) | UnrecognizedToken t -> 
            t
        | DecimalSeparatorToken ->  "."

type Expression = 
    | Number of decimal
    | Variable of string
    | Operator of OpType
    | Operation of Expression * OpType * Expression 
    | Group of Expression list
    | Unparsed of Token
