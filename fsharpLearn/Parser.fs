module fsharpLearn.Parser

open System
open System.Text.RegularExpressions
open fsharpLearn.Types
open Microsoft.FSharp.Core

let removeLastN nr col = 
    col
    |> List.take (col.Length - nr)
    |> List.ofSeq
let safeReduce f l = match l with
                     | [] -> []
                     | _ -> l |> List.reduce (f)

let mapReduce f state = List.mapFold f state >> fst >> safeReduce (@)

let removeLast = removeLastN 1

let (>=>) expr f =
    match expr with
    | Group(inner) -> Group(f inner)
    | Sqrt(Param(inner)) -> Sqrt(Param(f inner))
    | Sin(Param(inner)) -> Sin(Param(f inner))
    | _ -> expr

let parseToken token = 
    match token with
    | d when Regex.IsMatch(token, "^\d+(\.\d{1,2})?$") -> NumberToken(value = Decimal.Parse(d), token = token)
    | l when Regex.IsMatch(token, "[a-zA-Z]") -> LetterToken(token)
    | "+" -> OpToken(Plus)
    | "-" -> OpToken(Minus)
    | "*" -> OpToken(Multiply)
    | "^" -> OpToken(Power)
    | "/" -> OpToken(Divide)
    | "(" -> GroupStartToken 
    | ")" -> GroupEndToken
    | "." -> DecimalSeparatorToken
    | x -> UnrecognizedToken(x)

let parseTokens tokens = 
    tokens
    |> Seq.map parseToken
    |> List.ofSeq

let joinNumbers tokens = 
    tokens |> List.fold (fun tokens currentToken -> 
                            match tokens |> List.tryLast, currentToken with
                            | Some(NumberToken(_, prevToken)), NumberToken(_, token) -> 
                                removeLast tokens @ [ NumberToken(decimal (prevToken + token), prevToken + token) ]
                            | _, _ -> tokens @ [ currentToken ]) List.empty

let joinDecimals tokens = 
    tokens |> List.fold (fun tokens currentToken ->
                        match (tokens |> List.tryItem (tokens.Length - 2),  tokens |> List.tryLast, currentToken) with
                        | Some(NumberToken(_, t)), Some DecimalSeparatorToken, NumberToken(_, ct) -> 
                            let numberAsString = t + "." + ct
                            (tokens |> removeLastN 2) @ [ NumberToken(decimal numberAsString, numberAsString) ]
                        | _ -> tokens @ [ currentToken ]) List.empty

let parseFunctions tokens = 
    let rec loop tokens = 
        match tokens with
        | [] -> []
        | x :: tail ->
        match x with
        | GroupStartToken _ ->
            let functionToken = 
                let funName = tail 
                                |> List.mapFold(fun keepGoing token -> match (keepGoing, token) with | (true, LetterToken lt) -> (lt,true) | _ -> ("",false)) true
                                |> fst
                                |> List.rev
                                |> String.Concat
                                   
                match funName |> String.length with
                | 0 -> None
                | 1 -> Some (LetterToken(funName)) //this assumes there are no functions with only one letter... not sure if i can
                | _ -> Some (FunctionToken(funName))

            match functionToken with
            | Some ((FunctionToken t) as lt)
            | Some ((LetterToken t) as lt) -> 
                GroupStartToken :: lt :: loop (tail |> List.skip t.Length)
            | _ ->  GroupStartToken ::  loop tail
        | _ -> x :: (tail |> loop)
    
    tokens |> List.rev |> loop |> List.rev

let getTokensFromGroup tokens =
    tokens |> mapReduce (fun state x-> 
                            match state >= 1, x with
                                | false, _ -> ([],state)
                                | true, GroupEndToken(_) -> 
                                    if state >= 1 then 
                                        ([x],state-1) 
                                    else 
                                        ([x],state)
                                | true, GroupStartToken(_) -> 
                                        ([x], state+1)
                                | true, _ -> 
                                        ([x],state)) 1
        
let rec buildExpressions tokens = 
        match tokens with
        | [] -> []
        | x :: tail -> 
            match x with
            | NumberToken(v, _) ->
                Number(v) :: (tail |> buildExpressions)
            | LetterToken(v) -> 
                Variable(v) :: (tail |> buildExpressions)
            | GroupStartToken(_) -> 
                let tokensFromGroup = tail |> getTokensFromGroup
                let tailWithoutTokensFromGroup = tail |> List.skip tokensFromGroup.Length
                Group(buildExpressions tokensFromGroup) ::  buildExpressions tailWithoutTokensFromGroup
            | GroupEndToken(_) -> []
            | OpToken(t) -> Operator(t) :: buildExpressions tail
            | FunctionToken(t) -> 
                let tokensFromGroup = tail.Tail |> getTokensFromGroup
                let tailWithoutTokensFromGroup = tail.Tail |> List.skip tokensFromGroup.Length
                match t.ToLower() with
                | "sqrt" -> Sqrt (Param(buildExpressions tokensFromGroup))  :: buildExpressions tailWithoutTokensFromGroup
                | "sin" -> Sin (Param(buildExpressions tokensFromGroup))  :: buildExpressions tailWithoutTokensFromGroup
                | _ -> Unparsed(x) :: buildExpressions tail
            | UnrecognizedToken(_) | DecimalSeparatorToken -> 
                Unparsed(x) :: buildExpressions tail

let rec inferMultiplications expressions = 
    let getSqrt inner = Sqrt(Param(inferMultiplications inner))
    let getSin inner = Sin(Param(inferMultiplications inner))
    let matchCurrent current = match current with
                                | Group(inner) -> [Operator(Multiply); Group(inferMultiplications inner)], Some (Group(inferMultiplications inner))
                                | Sqrt(Param(inner)) -> [Operator(Multiply); getSqrt inner], Some (getSqrt inner)
                                | Sin(Param(inner)) -> [Operator(Multiply); getSin inner], Some (getSin inner)
                                | Number(_) | Variable(_) -> [Operator(Multiply); current], Some (current)
                                | Operator(_) | Operation(_) | Unparsed(_) -> [current], Some(current)
    
    expressions |> mapReduce (fun last current -> 
          match last with
          | None ->  
            match current with
            | Group(inner) -> [Group(inferMultiplications inner)], Some (Group(inferMultiplications inner))
            | Sqrt(Param(inner)) -> [getSqrt inner], Some (getSqrt inner)
            | Sin(Param(inner)) -> [getSin inner], Some (getSin inner)
            | Number(_) | Variable(_) | Operator(_) | Operation(_) | Unparsed(_) -> [current], Some(current)
          | Some(Variable(_)) | Some(Number(_)) | Some(Group(_)) | Some(Sqrt(_)) | Some(Sin(_)) -> matchCurrent current
          | Some(Operation(_)) | Some(Operator(_)) | Some(Unparsed(_)) -> [current], Some(current)) None  

let rec inferMissingZeroes expressions = 
    expressions |> 
    mapReduce (fun last current ->
                match last with
                | None | Some(Operator(_)) -> 
                    match current with 
                    | Operator(t2) when (t2 = Plus || t2 = Minus) -> [Number(0m); current], Some current
                    | Group(inner) -> [Group(inner |> inferMissingZeroes)], Some(Group(inner |> inferMissingZeroes))
                    | Sqrt(Param(exp)) -> [Sqrt(Param(inferMissingZeroes exp))], Some(Sqrt(Param(inferMissingZeroes exp)))
                    | Sin(Param(exp)) -> [Sin(Param(inferMissingZeroes exp))], Some(Sin(Param(inferMissingZeroes exp)))
                    | Variable(_) | Number(_) | Operation(_) | Unparsed(_) | Operator(_) | Sin(_)-> [current], Some current
                | Some(Variable(_)) | Some(Number(_)) | Some(Operator(_)) 
                | Some(Operation(_)) | Some(Unparsed(_)) | Some(Group(_)) | Some(Sqrt(_))
                | Some(Sin(_)) -> [current], Some current) None


let rec transformToOperations expressions=
    let r = 
        expressions |> List.fold(fun state item -> 
            match item with
            | Number(_) | Variable(_) | Group(_) | Sqrt(_) | Sin(_) | Operator(_) | Unparsed(_) ->
                match state with
                | (None, None, None) -> (Some (item >=> transformToOperations), None, None)
                | (Some left,None, None) -> ((Some left,Some item,None))
                | (Some left,Some (Operator(t)), None) -> ((Some left,Some (Operator(t)),(Some (item >=> transformToOperations))))
                | (Some a, Some (Operator(b)), Some c) -> (Some (Operation(a,b,c)), Some item, None)
                | _ -> (None, None, None)
            | Operation(_) -> (None, None, None)
            ) (None,None, None)
            
    match r with
    | Some a, Some (Operator(b)), Some c -> [Operation(a, b, c)]
    | Some a, None, None -> [a]
    | _ -> []

let rec rewriteWithPrecedence op =
    let operationPrecedence = function | Plus | Minus -> 0 | Multiply | Divide -> 1 | Power -> 2
    let isMoreImportantThan a b = operationPrecedence a > operationPrecedence b

    match op with 
    | Operation (left, op1, right) -> 
        match left |> rewriteWithPrecedence with 
        | Operation(l2, op2, r2) when op2 |> isMoreImportantThan op1 -> Operation (l2, op2, (Operation(r2, op1, right)))
        | x -> Operation(x, op1, right)
    | Group(inner) -> Group([(rewriteWithPrecedence (inner |> List.head))])
    | Sqrt(Param(inner)) -> Sqrt(Param([rewriteWithPrecedence (inner |> List.head)]))
    | Sin(Param(inner)) -> Sin(Param([rewriteWithPrecedence (inner |> List.head)]))
    | Number(_) | Variable(_) | Operator(_) 
    | Sqrt(_) | Sin(_) | Unparsed(_) -> op

let inferOperations expressions= 
    expressions |> transformToOperations |> List.head |> rewriteWithPrecedence