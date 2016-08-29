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
    let rec loop tokens = match tokens with
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
                | "sqrt" -> Sqrt (Group(buildExpressions tokensFromGroup))  :: buildExpressions tailWithoutTokensFromGroup
                | _ -> Unparsed(x) :: buildExpressions tail
            | UnrecognizedToken(_) | DecimalSeparatorToken -> 
                Unparsed(x) :: buildExpressions tail

let rec inferMultiplications expressions = 
    let getNewSqrt inner = match inner with | Group(expr) -> Sqrt(Group(inferMultiplications expr)) | _ -> Sqrt(inner)
    let matchCurrent current = match current with
                                | Group(inner) -> [Operator(Multiply); Group(inferMultiplications inner)], Some (Group(inferMultiplications inner))
                                | Sqrt(inner) -> [Operator(Multiply); getNewSqrt inner], Some (getNewSqrt inner)
                                | Number(_) | Variable(_) -> [Operator(Multiply); current], Some (current)
                                | Operator(_) | Operation(_) | Unparsed(_) -> [current], Some(current)
    
    expressions |> mapReduce (fun last current -> 
          match last with
          | None ->  
            match current with
            | Group(inner) -> [Group(inferMultiplications inner)], Some (Group(inferMultiplications inner))
            | Sqrt(inner) -> [getNewSqrt inner], Some (getNewSqrt inner)
            | Number(_) | Variable(_) | Operator(_) | Operation(_) | Unparsed(_) -> [current], Some(current)
          | Some(Variable(_)) | Some(Number(_)) | Some(Group(_)) | Some(Sqrt(_)) -> matchCurrent current
          | Some(Operation(_)) | Some(Operator(_)) | Some(Unparsed(_)) -> [current], Some(current)) None  

let rec inferMissingZeroes expressions = 
    expressions |> 
    mapReduce (fun last current ->
                match last with
                | None | Some(Operator(_)) -> 
                    match current with 
                    | Operator(t2) when (t2 = Plus || t2 = Minus) -> [Number(0m); current], Some current
                    | Group(inner) -> [Group(inner |> inferMissingZeroes)], Some(Group(inner |> inferMissingZeroes))
                    | Sqrt(inner) -> 
                        let newExpr = match inner with | Group(exp) -> Sqrt(Group(inferMissingZeroes exp)) | _ -> Sqrt(inner)
                        [newExpr], Some(newExpr)
                    | Variable(_) | Number(_) | Operation(_) | Unparsed(_) | Operator(_)-> [current], Some current
                | Some(Variable(_)) | Some(Number(_)) | Some(Operator(_)) 
                | Some(Operation(_)) | Some(Unparsed(_)) | Some(Group(_)) | Some(Sqrt(_))-> [current], Some current) None

let inferOperations expressions= 
    let rec inferOperators opTypes xs =
        match xs with
        | [] -> []
        | current :: tail -> 
            match (tail |> List.tryHead), tail |> List.tryItem 1 with
            | Some (Operator(opType)), Some right when opTypes |> List.contains opType->
                let getExpr current = match current with
                                        | Group x -> (Group(inferLogic x))
                                        | Sqrt (Group inner) -> Sqrt(Group(inferLogic inner))
                                        | _ -> current

                let op = Operation (getExpr current, opType, getExpr right)
                let next = tail |> List.skip 2 |> (@) [op]
                (inferOperators opTypes next)
            | _ -> current :: (tail |> inferOperators opTypes)
    and inferLogic = inferOperators[Power] >> inferOperators [Multiply; Divide] >> inferOperators [Plus; Minus]
        
    inferLogic expressions
