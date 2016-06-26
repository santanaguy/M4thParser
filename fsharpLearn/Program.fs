module fsharpLearn.main 
open System
open System.Collections
open System.Linq
open System.Text.RegularExpressions
open fsharpLearn.Types
open fsharpLearn.Parser

[<EntryPoint>]
let main argv2 = 
    let input = "(1 * (1+2))"
    let inputSanitized = input.ToCharArray() |> Seq.map (fun c -> c.ToString())
    
    let parsed = inputSanitized
                |> parseTokens
                |> joinNumbers
                |> buildExpressions List.empty
//                |> Seq.fold inferMultiplications List.empty

    printfn "%s \r\n %A" input parsed
    Console.ReadLine() |> ignore
    0 // return an integer exit code
