module fsharpLearn.mainModule 
open System
open System.Collections
open System.Linq
open System.Text.RegularExpressions
open fsharpLearn.Types
open fsharpLearn.Parser

let getTokens = let toCharArray (s:string) = s.ToCharArray() 
                String.filter(fun c -> c |> Char.IsWhiteSpace = false)
                >> toCharArray
                >> Seq.map (fun c -> c.ToString())
                >> parseTokens
                >> joinNumbers
                >> joinDecimals
                >> parseFunctions

let parseInput = getTokens >> buildExpressions >> inferMultiplications >> inferMissingZeroes >> inferOperations
  
[<EntryPoint>]
let main argv2 = 
    let rec takeInput() = 
        match Console.ReadLine() with
            | "exit" -> 0
            | x -> 
                let parsed = parseInput x

                printfn "%s \r\n %A" x parsed
                takeInput()
    takeInput() |> ignore    
    0 
