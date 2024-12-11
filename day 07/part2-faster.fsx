#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20""".Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parseLine (line : string) =
    let [|result;operands|] = line.Split(": ")
    let operandsParsed = operands.Split(" ") |> Seq.map int64 |> Seq.toList
    (int64 result, operandsParsed)
    
let rec evaluate (result : int64) operands =
    match operands with
    | [x] -> x = result
    | h :: t when result % h = 0 && evaluate (result / h) t -> true
    | h :: t when result >= h && evaluate (result - h) t -> true
    | h :: _ when (result |> string |> Seq.length <= (h |> string |> Seq.length)) -> false
    | h :: _ when result |> string |> (_.EndsWith(string h)) |> not -> false
    | h :: t ->
         let s = result |> string
         let sl = s |> Seq.length
         let hl = h |> string |> Seq.length
         let sub = s.Substring(0,sl-hl)
         printfn $"result {result} h {h} t {t} sub {sub}"
         let subresult = sub |> int64
         evaluate subresult t

let couldBeTrue (result, operands) =
    evaluate result (operands |> List.rev)
   
let parsed = input |> List.map parseLine

// #time //Real: 00:00:00.069, CPU: 00:00:00.031, GC gen0: 1, gen1: 0, gen2: 0
let result = 
    parsed
    |> List.filter couldBeTrue
    |> List.sumBy fst    

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()