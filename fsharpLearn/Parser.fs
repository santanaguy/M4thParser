module fsharpLearn.Parser

open System
open System.Collections
open System.Linq
open System.Text.RegularExpressions
open fsharpLearn.Types

let (|IsDigit|_|) d = 
    if Regex.IsMatch(d, "\d") then Some(Int32.Parse(d))
    else None

let (|Digit|Op|IncompleteToken|Unrecognized|) token = 
    match token with
    | "+" -> Op(Operator(Plus, "+"))
    | "-" -> Op(Operator(Minus, "-"))
    | "*" -> Op(Operator(Multiply, "*"))
    | "/" -> Op(Operator(Divide, "/"))
    | "(" -> IncompleteToken(GroupStart, "(")
    | ")" -> IncompleteToken(GroupEnd, ")")
    | IsDigit d -> Digit(Number(value = d, token = token))
    | _ -> Unrecognized

let getAllExceptLast (col : 'a list) = (col.Take(col.Length - 1)) |> List.ofSeq
let mergeNumber (value1 : int, token1 : string) (value2 : int, token2 : string) = 
    Number(int (token1 + token2), token1 + token2)

let foldNumbers (list : Token list) current = 
    match list, list.LastOrDefault(), current with
    | [], _, _ -> list @ [ current ]
    | _, Number(prevValue, prevToken), Number(value, token) -> 
        getAllExceptLast list @ [ mergeNumber (prevValue, prevToken) (value, token) ]
    | _, _, _ -> list @ [ current ]

let groupGroups (groups, currentGroup) currentToken = 
    match currentToken with
    | GroupStart(_) -> 
        let newgroup = List.empty
        (groups @ newgroup, newgroup)
    | GroupEnd(_) -> (groups, groups)

let parseToken token = 
    match token with
    | Digit d -> d
    | Op op -> op
    | IncompleteToken t -> MissingParsing t
    | _ -> Variable(value = token, token = token)

let parseTokens tokens = 
    tokens
    |> Seq.map parseToken
    |> List.ofSeq

let joinNumbers = List.fold foldNumbers List.empty

let inferMultiplications (list : Token list) current = 
    match list, list.LastOrDefault(), current with
    | [], _, _ -> list @ [ current ]
    | _, Number(_, _), Variable(_, _) | _, Variable(_, _), Number(_, _) | _, Variable(_, _), Variable(_, _) -> 
        list @ [ Operator(Multiply, "*")
                 current ]
    | _, _, _ -> list @ [ current ]

let buildGroups (list : Token list) current