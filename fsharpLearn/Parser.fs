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
            | OpToken(t,_) -> updateAndMoveNext exps (Operator(t)) tail
            | UnrecognizedToken(_) | DecimalSeparatorToken -> 
                updateAndMoveNext exps (Unparsed(x)) tail

    and updateAndMoveNext exps newExp tail = 
        tail |> build (exps @ [ newExp ])

    build List.empty tokens 

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
                            
let rec inferMissingZeroes last expressions = 
    match expressions with
    | [] -> []
    | x :: tail -> match last, x with
                    | None, Operator(t) when t = Plus || t = Minus -> 
                        Number(0m) :: x :: (tail |> inferMissingZeroes (Some x))
                    | Some(Operator(t1)), Operator(t2) 
                        when (t1 = Plus || t1 = Minus) && (t2 = Plus || t2 = Minus) ->
                            Number(0m) :: x :: (tail |> inferMissingZeroes (Some x))
                    | _, Group(inner) -> 
                        let grp =Group(inner |> inferMissingZeroes None)
                        grp :: (tail |> inferMissingZeroes (Some grp))
                    | _ -> 
                        x :: (tail |> inferMissingZeroes (Some x))
    
let parseOperators expressions= 
    let rec inferOperators opTypes exps xs =
        match xs with
        | [] -> exps
        | current :: tail -> 
            let (+>) = addAndMoveNext (inferOperators opTypes exps) tail 
            match current, (tail |> List.tryHead), tail |> List.tryItem 1 with
            | left, Some (Operator(opType)), Some right when opTypes |> List.contains opType->
                (+>) left opType right
            | _ -> 
                tail |> inferOperators opTypes (exps @ [current])
    and inferLogic = inferOperators[Power] List.empty >> inferOperators [Multiply; Divide] List.empty >> inferOperators [Plus; Minus] List.empty
    and addAndMoveNext continueWith tail left opType right = 
        let getExpr expression= match expression with
                                | Group x -> (Group(inferLogic x))
                                | _ -> expression

        let op = Operation (getExpr left, opType, getExpr right)
        let next = tail |> List.skip 2 |> (@) [op]
        next |> continueWith
        
    inferLogic expressions |> List.head
