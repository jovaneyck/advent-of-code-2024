#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines "day 00/input.txt"
    |> List.ofSeq

let example =
    """hehe""".Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()