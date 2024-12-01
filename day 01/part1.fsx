#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """3   4
4   3
2   5
1   3
3   9
3   3""".Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse (lines : string list) =
    lines
    |> List.map (fun line -> line.Split("   ") |> Seq.map int |> Seq.toList)
    |> List.transpose

let [first;second] = parse input
let sortedFirst = first |> List.sort
let sortedSecond = second |> List.sort

List.zip sortedFirst sortedSecond
|> List.map (fun (x,y) -> abs (x - y))
|> List.sum


let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()