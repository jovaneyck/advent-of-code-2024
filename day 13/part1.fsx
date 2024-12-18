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
    let xA = int mA.Groups[1].Value
    let yA = int mA.Groups[2].Value
    let xB = int mB.Groups[1].Value
    let yB = int mB.Groups[2].Value
    let xP = int mP.Groups[1].Value
    let yP = int mP.Groups[2].Value
    (xA,xB,xP),(yA,yB,yP)

let parseMachines (input : string) =
     input.Split("\n\n")
     |> Array.map (fun machine -> machine.Split("\n") |> parse)
     |> Array.toList

let solve machine =
    let ((A,B,C),(D,E,F)) = machine
    let a = (decimal ((B * F) - (C * E))) / (decimal ((B*D) - (A*E)))
    if a <> round a then
        None
    else
        let intA = int a
        let b = (C - A*intA) / B
        
        Some (intA,b)

let machines = parseMachines input
let solutions = machines |> List.choose solve
let result = solutions |> List.sumBy (fun (a,b) -> 3*a + b)

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()