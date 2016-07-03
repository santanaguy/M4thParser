module fsharpLearn.main 
open System
open System.Collections
open System.Linq
open System.Text.RegularExpressions
open fsharpLearn.Types
open fsharpLearn.Parser

[<EntryPoint>]
let main argv2 = 
    let rec takeInput() = 
       match Console.ReadLine() with
            | "exit" -> 0
            | x -> 
                let toCharArray (s:string) = s.ToCharArray() 
                let parsed = x |> toCharArray
                                |> Seq.map (fun c -> c.ToString())
                                |> parseTokens
                                |> joinNumbers
                                |> joinDecimals
                                |> buildExpressions
                                |> inferMultiplications
                                |> inferMissingZeroes

                printfn "%s \r\n %A" x parsed
                takeInput()
    takeInput() |> ignore    
    0 
