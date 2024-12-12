//For part 2:
//1. we note that order of the elements does not matter
//2. we note that there's a lot of "duplicate" numbers in the 75 rounds of blinking
//Approach: put every distinct number in a Counter and keep track of the number of times it occurs and "blink" over that data structure
#r "nuget: Unquote"
open Swensen.Unquote

type Number = Number of uint64
type Count = Count of uint64
type Counter = (Number * Count) list

module Counter =
    let init = Map.empty
    
    let fromTuples tuples : Counter =
        tuples
        |> List.groupBy fst
        |> List.map (fun (Number n, occs) ->
            (Number n,
             (occs |> Seq.map snd |> Seq.map (fun (Count c) -> uint64 c) |> Seq.reduce (+)) |> Count))
        
    let fromList list : Counter =
        list
        |> List.groupBy id
        |> List.map (fun (nb, occs) -> (nb, occs |> Seq.length))
        |> List.map (fun (n,c) -> Number n, Count (uint64 c))
        
    let toList (counter : Counter) = counter
    
    let total (counter : Counter) =
        counter |> Seq.sumBy (snd >>(fun (Count c) -> c))
        
    let collect f (counter : Counter) =
        counter
        |> toList
        |> List.collect (fun (Number n,c) -> f n |> List.map (fun n -> (Number n, c)))
        |> fromTuples

let input =
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""

let example =
    """125 17"""

let parse (input : string) =
    input.Split(" ")
    |> Seq.map uint64
    |> Seq.toList

let (|Zero|_|) stone : unit option =
    if stone = 0UL then Some () else None
let (|EvenNumberDigits|_|) stone : uint64 list option =
    let len = stone |> string |> String.length
    let (q,r) = (len/2,len%2)
    if r = 0
    then
        let o = pown 10 q |> uint64
        Some [stone/o; stone%o]
    else None
 
let applyRuleTo stone =
    (*
        If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
        If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
        If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
    *)
    match stone with
    | Zero -> [1UL]
    | EvenNumberDigits split -> split
    | _ -> [stone * 2024UL]        

let blink (stones : Counter) : Counter =
    stones |> Counter.collect applyRuleTo
    
let blinks (stones : Counter) =
    Seq.unfold (fun stones -> Some (stones,blink stones)) stones 

// #time
// Real: 00:00:00.209, CPU: 00:00:00.296, GC gen0: 2, gen1: 1, gen2: 0
let stones = input |> parse |> Counter.fromList
let blinked = stones |> blinks |> Seq.item 75
let result = blinked |> Counter.total

let run () =
    printf "Testing.."
    test <@ example |> parse |> Counter.fromList |> blinks |> Seq.item 75 |> Counter.total = 65601038650482UL @>
    printfn "...done!"

run ()