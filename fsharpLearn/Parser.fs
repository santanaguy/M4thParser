module fsharpLearn.Parser

open System
open System.Text.RegularExpressions
open fsharpLearn.Types

let removeLast col = 
    col
    |> List.take (col.Length - 1)
    |> List.ofSeq

let foldNumbers tokens currentToken = 
    match tokens, List.tryLast tokens, currentToken with
    | [], _, _ -> tokens @ [ currentToken ]
    | _, Some(NumberToken(prevValue, prevToken)), NumberToken(value, token) -> 
        removeLast tokens @ [ NumberToken(int (prevToken + token), prevToken + token) ]
    | _, _, _ -> tokens @ [ currentToken ]

let parseToken token = 
    match token with
    | d when Regex.IsMatch(token, "\d") ->  NumberToken(value = Int32.Parse(d), token = token)
    | "+" -> OperatorToken(Plus, token)
    | "-" -> OperatorToken(Minus, token)
    | "*" -> OperatorToken(Multiply, token)
    | "/" -> OperatorToken(Divide, token)
    | "(" -> GroupStartToken "("
    | ")" -> GroupEndToken ")"
    | " " -> SpaceToken(" ")
    | _ -> VariableToken(value = token, token = token)

let parseTokens tokens = 
    tokens
    |> Seq.map parseToken
    |> List.ofSeq

let joinNumbers = List.fold foldNumbers List.empty
                       
let rec buildExpressions current xs = 
    let addExpr expr current xs= 
        let updated = current @ [expr]
        xs |> buildExpressions updated

    match xs with
        | [] -> List.empty
        | x::tl -> match x with
                    | NumberToken(v, _) -> 
                        addExpr (Number(v)) current tl
                    | VariableToken(v, _) -> 
                        addExpr (Variable(v)) current tl
                    | GroupStartToken(_) ->
                        current @ [Group(tl |> buildExpressions current)]
                    | GroupEndToken(_) ->
                        current
                    | OperatorToken(_,_) | SpaceToken(_) | UnrecognizedToken(_) -> 
                        addExpr (NotYetParsed(x)) current tl
