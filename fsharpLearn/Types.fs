module fsharpLearn.Types

type OperatorBase = 
    | Plus
    | Minus
    | Multiply
    | Divide

type NotCompletelyParsedToken = 
    | GroupStart
    | GroupEnd

type Token = 
    | Number of value : int * token : string
    | Variable of value : string * token : string
    | Operator of value : OperatorBase * token : string
    | MissingParsing of NotCompletelyParsedToken * token : string
    | Unrecognized of token : string
    member x.getExpression() : string = 
        match x with
        | Number(_, t) | Variable(_, t) | Operator(_, t) | MissingParsing(_, t) | Unrecognized(t) -> "\r\n" + t
