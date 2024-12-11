#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""

let example =
    """125 17"""

let parse (input : string) =
    input.Split(" ")
    |> Seq.map uint64
    |> Seq.toList

let applyRuleTo stone =
    (*
        If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
        If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
        If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
    *)
    if stone = 0UL then [1UL]
    else
        let asString = stone |> string
        if asString.Length % 2 = 0
        then
            asString |> Seq.splitInto 2 |> Seq.map (Seq.map string >> String.concat "") |> Seq.map uint64 |> Seq.toList
        else
            [stone * 2024UL]
let blink stones =
    stones |> List.collect applyRuleTo
  
let rec repeat n acc =
    if n = 0
    then acc
    else
        repeat (n - 1) (blink acc)
   
let stones = input |> parse
// #time
let blinked = stones |> repeat 25
let result = blinked |> Seq.length

let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()