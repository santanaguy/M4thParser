module fsharpLearn.Types

type OpType = 
    | Plus
    | Minus
    | Multiply
    | Divide
    | Power

type Token = 
    | NumberToken of value : decimal * token : string
    | LetterToken of value : string
    | FunctionToken of value : string
    | OpToken of value : OpType 
    | GroupStartToken 
    | GroupEndToken 
    | DecimalSeparatorToken
    | UnrecognizedToken of token : string

type Expression = 
    | Number of decimal
    | Variable of string
    | Operator of OpType
    | Sqrt of Expression
    | Operation of Expression * OpType * Expression
    | Group of Expression list
    | Unparsed of Token
