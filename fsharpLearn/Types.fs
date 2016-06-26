module fsharpLearn.Types

type OperatorType = 
    | Plus
    | Minus
    | Multiply
    | Divide

type Token = 
    | NumberToken of value : int * token : string
    | VariableToken of value : string * token : string
    | OperatorToken of value : OperatorType * token : string
    | GroupStartToken of token : string
    | GroupEndToken of token : string
    | SpaceToken of token : string
    | UnrecognizedToken of token : string
    member x.getExpression() = 
        match x with
        | NumberToken(_, t) | VariableToken(_, t) | OperatorToken(_, t) | GroupStartToken(t) | GroupEndToken(t) | UnrecognizedToken t | SpaceToken t -> 
            "\r\n" + t

type Expression = 
    | Number of int
    | Variable of string
    | Operator of Expression * Expression * OperatorType
    | Group of Expression list
    | NotYetParsed of Token
