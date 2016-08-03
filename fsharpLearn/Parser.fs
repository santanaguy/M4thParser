﻿module fsharpLearn.Parser

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
    | "." -> DecimalSeparatorToken
    | "x" | "y" -> VariableToken(value = token, token = token)
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

let getTokensFromGroup tokens  =
    let rec getTokens depth tokens xs =
        match xs with
        | [] -> tokens
        | x :: tail -> 
            match x with
                | GroupEndToken(_) -> 
                    let updated = tokens @ [ x ]
                    if depth > 1 then 
                        tail |> getTokens (depth - 1) updated
                    else 
                        updated
                | GroupStartToken(_) -> 
                        tail |> getTokens (depth + 1) (tokens @ [ x ]) 
                | _ -> 
                        tail |> getTokens depth (tokens @ [ x ]) 
        
    tokens |> getTokens 1 List.empty

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
                let tokensFromGroup = tail |> getTokensFromGroup
                let newGroupContent = tokensFromGroup |> build List.empty 
                let newGroup = Group(newGroupContent)
                let tailWithoutTokensFromGroup = tail |> List.skip tokensFromGroup.Length
                updateAndMoveNext exps newGroup tailWithoutTokensFromGroup
            | GroupEndToken(_) -> exps
            | OperatorToken(_, _) | UnrecognizedToken(_) | DecimalSeparatorToken -> 
                updateAndMoveNext exps (NotYetParsed(x)) tail

    and updateAndMoveNext exps newExp tail = 
        tail |> build (exps @ [ newExp ])

    build List.empty tokens 

let inferMultiplications tokens =
    let fold acc current = 
        match acc |> List.tryLast, current  with
        | Some(NumberToken(_)),VariableToken(_)
        | Some(NumberToken(_)),GroupStartToken(_)
        | Some(VariableToken(_)), NumberToken(_)
        | Some(VariableToken(_)), VariableToken(_)
        | Some(VariableToken(_)), GroupStartToken(_) 
        | Some(GroupEndToken(_)), NumberToken(_) 
        | Some(GroupEndToken(_)), VariableToken(_)
        | Some(GroupEndToken(_)), GroupStartToken(_)->
            acc @ [ OperatorToken(Multiply, "*"); current]
        | _, _->
            acc @ [current]
    tokens |> List.fold fold List.empty
                            
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
                            let updated = exps @ [Group(inner |> infer List.empty)]
                            tail |> infer updated
                        | _ -> 
                            tail |> infer (exps @ [x])
    
    expressions |> infer List.empty

let parseOperators opType expressions= 
    let rec infer opType exps xs =
        match xs with
        | [] -> exps
        | x :: tail -> match x, tail |> List.tryHead, tail |> List.tryItem 1 with
                        | exp1, Some (NotYetParsed(OperatorToken(t, _))), Some exp2 when t = opType->
                            let op = Operator(exp1, opType, exp2)
                            let next = tail |> List.skip 2 |> (@) [op] 
                            next |> infer opType exps
                        | _,_,_ -> 
                            tail |> infer opType (exps @ [x])
                        

    expressions |> infer opType List.empty