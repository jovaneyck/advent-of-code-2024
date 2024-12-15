#r "nuget: Unquote"
open Swensen.Unquote
open System.Text.RegularExpressions

//algebra time, let's solve some 2x2 linear equation systems (see excalidraw)
(*
   system has a solution if a is an integer?
   
    Aa + Bb = C
    Da + Eb = F
    
    a = ((B * F) - (C * E)) / ( (B*D) - (A*E))  
    b = (C - Aa) / B
*)

let input =
    (System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt""").Replace("\r\n","\n")
    

let example =
    """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""

let regA = Regex("Button A: X\+(\d*), Y\+(\d*)")
let regB = Regex("Button B: X\+(\d*), Y\+(\d*)")
let regPrize = Regex("Prize: X=(\d*), Y=(\d*)")

let parse machine =
    let [|a;b;prize|] = machine
    let mA = regA.Match(a)
    let mB = regB.Match(b)
    let mP = regPrize.Match(prize)
    let xA = decimal mA.Groups[1].Value
    let yA = decimal mA.Groups[2].Value
    let xB = decimal mB.Groups[1].Value
    let yB = decimal mB.Groups[2].Value
    let xP = 10000000000000m + decimal mP.Groups[1].Value
    let yP = 10000000000000m + decimal mP.Groups[2].Value
    (xA,xB,xP),(yA,yB,yP)

let parseMachines (input : string) =
     input.Split("\n\n")
     |> Array.map (fun machine -> machine.Split("\n") |> parse)
     |> Array.toList

let solve machine =
    let ((A,B,C),(D,E,F)) = machine
    let a = ((B * F) - (C * E)) / ((B*D) - (A*E))
    let b = (C - A*a) / B
    if a % 1m = 0m && b % 1m = 0m
    then Some (a,b)
    else None

let machines = parseMachines input
let solutions = machines |> List.choose solve
let result = solutions |> List.sumBy (fun (a,b) -> 3m*a + b)

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()