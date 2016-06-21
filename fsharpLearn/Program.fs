module fsharpLearn.main 
open System
open System.Collections
open System.Linq
open System.Text.RegularExpressions
open fsharpLearn.Types
open fsharpLearn.Parser

[<EntryPoint>]
let main argv2 = 
    let input = "21 + (-22x + 155xx) - bx".Replace(" ", "").ToCharArray() |> Seq.map (fun c -> c.ToString())
    
    let parsed = input
                |> parseTokens
                |> joinNumbers
                |> Seq.fold inferMultiplications List.empty

    parsed
    |> List.fold (fun acc x -> acc + x.getExpression()) ""
    |> printfn "%A"
    |> ignore
    
    printfn "%A" parsed
    Console.ReadLine() |> ignore
    0 // return an integer exit code
