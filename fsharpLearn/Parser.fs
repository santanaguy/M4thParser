module fsharpLearn.Parser

open System
open System.Text.RegularExpressions
open fsharpLearn.Types
open Microsoft.FSharp.Core

let removeLastN nr col = 
    col
    |> List.take (col.Length - nr)
    |> List.ofSeq
let mapReduce f state = List.mapFold f state >> fst >> List.reduce(@)

let removeLast = removeLastN 1

let parseToken token = 
    match token with
    | d when Regex.IsMatch(token, "^\d+(\.\d{1,2})?$") -> NumberToken(value = Decimal.Parse(d), token = token)
    | "+" -> OpToken(Plus, token)
    | "-" -> OpToken(Minus, token)
    | "*" -> OpToken(Multiply, token)
    | "^" -> OpToken(Power, token)
    | "/" -> OpToken(Divide, token)
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
            | VariableToken(v, _) -> 
                Variable(v) :: (tail |> buildExpressions)
            | GroupStartToken(_) -> 
                let tokensFromGroup = tail |> getTokensFromGroup
                let tailWithoutTokensFromGroup = tail |> List.skip tokensFromGroup.Length
                Group(buildExpressions tokensFromGroup) ::  buildExpressions tailWithoutTokensFromGroup
            | GroupEndToken(_) -> []
            | OpToken(t,_) -> Operator(t) :: buildExpressions tail
            | UnrecognizedToken(_) | DecimalSeparatorToken -> 
                Unparsed(x) :: buildExpressions tail

let inferMultiplications tokens = 
    tokens |> mapReduce (fun last item -> 
        match last, item  with
        | NumberToken(_),VariableToken(_)
        | NumberToken(_),GroupStartToken(_)
        | VariableToken(_), NumberToken(_)
        | VariableToken(_), VariableToken(_)
        | VariableToken(_), GroupStartToken(_) 
        | GroupEndToken(_), NumberToken(_) 
        | GroupEndToken(_), VariableToken(_)
        | GroupEndToken(_), GroupStartToken(_)->
             [OpToken(Multiply, "*"); item],item
        | _, _->
            [item],item) tokens.Head
                            
let rec inferMissingZeroes expressions = 
    expressions |> 
    mapReduce (fun last item ->
                match last, item with
                    | None, Operator(t) when t = Plus || t = Minus -> 
                        [Number(0m); item], Some item
                    | Some(Operator(t1)), Operator(t2) 
                        when (t1 = Plus || t1 = Minus) && (t2 = Plus || t2 = Minus) ->
                         [Number(0m); item], Some item
                    | _, Group(inner) -> 
                        let grp =Group(inner |> inferMissingZeroes)
                        [grp], Some grp 
                    | _ -> 
                        [item], Some item) None

let parseOperators expressions= 
    let rec inferOperators opTypes xs =
        match xs with
        | [] -> []
        | current :: tail -> 
            match (tail |> List.tryHead), tail |> List.tryItem 1 with
            | Some (Operator(opType)), Some right when opTypes |> List.contains opType->
                let getExpr current = match current with
                                        | Group x -> (Group(inferLogic x))
                                        | _ -> current

                let op = Operation (getExpr current, opType, getExpr right)
                let next = tail |> List.skip 2 |> (@) [op]
                (inferOperators opTypes next)
            | _ -> current :: (tail |> inferOperators opTypes)
    and inferLogic = inferOperators[Power] >> inferOperators [Multiply; Divide] >> inferOperators [Plus; Minus]
        
    inferLogic expressions |> List.head