module fsharpLearn.Parser

open System
open System.Text.RegularExpressions
open fsharpLearn.Types
open Microsoft.FSharp.Core

let removeLastN nr col = 
    col
    |> List.take (col.Length - nr)
    |> List.ofSeq

let removeLast = removeLastN 1

let parseToken token = 
    match token with
    | d when Regex.IsMatch(token, "^\d+(\.\d{1,2})?$") -> NumberToken(value = Decimal.Parse(d), token = token)
    | "+" -> OperatorToken(Plus, token)
    | "-" -> OperatorToken(Minus, token)
    | "*" -> OperatorToken(Multiply, token)
    | "/" -> OperatorToken(Divide, token)
    | "(" -> GroupStartToken "("
    | ")" -> GroupEndToken ")"
    | " " -> SpaceToken(" ")
    | "." -> DecimalSeparatorToken
    | "x" -> VariableToken(value = token, token = token)
    | x -> UnrecognizedToken(x)

let parseTokens tokens = 
    tokens
    |> Seq.map parseToken
    |> List.ofSeq

let joinNumbers tokens = 
    let foldNumbers tokens currentToken = 
        match tokens |> List.tryLast, currentToken with
        | Some(NumberToken(_, prevToken)), NumberToken(_, token) -> 
            removeLast tokens @ [ NumberToken(decimal (prevToken + token), prevToken + token) ]
        | _, _ -> tokens @ [ currentToken ]
    tokens |> List.fold foldNumbers List.empty

let joinDecimals tokens = 
    let foldDecimals tokens currentToken = 
        match (tokens |> List.tryLast, tokens |> List.tryItem (tokens.Length - 2), currentToken) with
        | Some(DecimalSeparatorToken), Some(NumberToken(_, t)), NumberToken(_, ct) -> 
            let numberAsString = t + "." + ct
            (tokens |> removeLastN 2) @ [ NumberToken(decimal numberAsString, numberAsString) ]
        | _, _, _ -> tokens @ [ currentToken ]
    tokens |> List.fold foldDecimals List.empty

let buildExpressions tokens = 
    let rec build exps xs = 
        match xs with
        | [] -> exps
        | x :: tail -> 
            match x with
            | NumberToken(v, _) ->
                updateAndMoveNext exps (Number(v)) tail
            | VariableToken(v, _) -> 
                updateAndMoveNext exps (Variable(v)) tail
            | GroupStartToken(_) -> 
                let updated = exps @ [Group(tail |> build List.empty)]
                let isPartOfGroup e = match e with | GroupEndToken(_) -> false | _ -> true
                let tailWithoutTokensFromGroup = tail |> List.skipWhile isPartOfGroup |> List.tail
                
                tailWithoutTokensFromGroup |> build updated
            | GroupEndToken(_) -> exps
            | OperatorToken(_, _) | SpaceToken(_) | UnrecognizedToken(_) | DecimalSeparatorToken -> 
                updateAndMoveNext exps (NotYetParsed(x)) tail

    and updateAndMoveNext exps newExp tail= 
        let updated = exps @ [ newExp ]
        (tail |> build updated)

    build List.empty tokens

let inferMultiplications expressions=
    let folder acc current = 
        match acc |> List.tryLast, current with
        | Some(Number(_)), Variable(_) -> 
            acc @ [ NotYetParsed(OperatorToken(Multiply, "*")); current]
        | Some(Group(_)), Variable(_) -> 
            acc @ [ NotYetParsed(OperatorToken(Multiply, "*")); current]
        | Some(Group(_)), Group(_) -> 
            acc @ [ NotYetParsed(OperatorToken(Multiply, "*")); current]
        | _, _ ->
            acc @ [current]

    expressions |> List.fold folder List.empty

let inferMissingZeroes expressions = 
    let rec infer exps xs =
        match xs with
        | [] -> exps
        | x :: tail -> match exps |> List.tryLast, x with
                        | None, NotYetParsed(OperatorToken(_)) -> 
                            let updated = exps @ [Number(0m); x]
                            tail |> infer updated
                        | Some(NotYetParsed(OperatorToken(_))), NotYetParsed(OperatorToken(_)) -> 
                            let updated = exps @ [Number(0m); x]
                            tail |> infer updated
                        | _, Group(inner) -> 
                            exps @ [Group(inner |> infer List.empty)]
                        | _ -> 
                            tail |> infer (exps @ [x])

    
    expressions |> infer List.empty
//    let rec folder acc current = 
//        match acc |> List.tryLast, current with
//        | None, _ -> acc @ [current]
//        | Some(NotYetParsed(OperatorToken(_))), NotYetParsed(OperatorToken(_)) -> acc @ [Number(0m); current]
//        | Some(Group(_)), _ -> acc @ [ |> List.fold folder acc]
//
//    expressions |> List.fold folder List.empty

//let buildOperators expressions = 
//    let folder acc current = 
//        match acc |> List.tryLast, current with
//        | None, _ -> acc @ [current]
//        | Some(Number(_) as n), NotYetParsed(OperatorToken(op, _)) -> acc @ [Operator(n, TempPlaceholder, op)]
//        | Some(Variable(_) as v), NotYetParsed(OperatorToken(op, _)) -> acc @ [Operator(v, TempPlaceholder, op)]
//        | Some(Group(_) as v), NotYetParsed(OperatorToken(op, _)) -> acc @ [Operator(v, TempPlaceholder, op)]
//        
//        | Some(Operator(_,r, op1)), NotYetParsed(OperatorToken(op, _)) -> acc @ [Operator(v, TempPlaceholder, op)]
//        | Some(NotYetParsed(OperatorToken(_))), _ -> acc @ [current]
//
//    expressions |> List.fold folder List.empty