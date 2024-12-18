#r "nuget: Unquote"
open Swensen.Unquote

let input =
    (System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt""").Replace("\r\n","\n")

let example =
    """Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"""

let POINTER_STEP_SIZE = 2UL

type Computer = { A : uint64; B: uint64; C: uint64; Pointer: uint64; Output : uint64 list }

module Computer =
    let create a b c = { A = a; B = b; C = c; Pointer = 0UL; Output = [] }

type Operator =
    | Adv
    | Bxl
    | Bst
    | Jnz
    | Bxc
    | Out
    | Bdv
    | Cdv

let toNum =
    function
    | Adv -> 0UL
    | Bxl -> 1UL
    | Bst -> 2UL
    | Jnz -> 3UL
    | Bxc -> 4UL
    | Out -> 5UL
    | Bdv -> 6UL
    | Cdv -> 7UL
    
let valueOfComboOperand computer =
    function
        | 0UL -> 0UL
        | 1UL -> 1UL
        | 2UL -> 2UL
        | 3UL -> 3UL
        | 4UL -> computer.A
        | 5UL -> computer.B
        | 6UL -> computer.C
        | unexpected -> failwith $"Unexpected combo operand {unexpected}"
 
let adv computer (operand : uint64) =
    let nom = computer.A
    let denom = System.Math.Pow(float 2m, float (valueOfComboOperand computer operand)) |> uint64
    let result = nom / denom
    { computer with A = result }
    
let bdv computer (operand : uint64) =
    let nom = computer.A
    let denom = System.Math.Pow(float 2m, float (valueOfComboOperand computer operand)) |> uint64
    let result = nom / denom
    { computer with B = result }
    
let cdv computer (operand : uint64) =
    let nom = computer.A
    let denom = System.Math.Pow(float 2m, float (valueOfComboOperand computer operand)) |> uint64
    let result = nom / denom
    { computer with C = result }
        
let bxl computer operand =
    let result = computer.B ^^^ operand
    { computer with B = result }
    
let bst computer operand =
    let result = (valueOfComboOperand computer operand) % 8UL
    { computer with B = result }
    
let jnz computer operand =
    if computer.A = 0UL
    then {computer with Pointer = computer.Pointer + POINTER_STEP_SIZE}
    else { computer with Pointer = operand }

let bxc computer =
    let result = computer.B ^^^ computer.C
    { computer with B = result }

let out computer operand =
    let result = (valueOfComboOperand computer operand) % 8UL
    { computer with Output = result :: computer.Output }

let applyInstruction computer operator operand =
    let pointer computer = { computer with Pointer = computer.Pointer + POINTER_STEP_SIZE }
    match operator with
    | 0UL -> adv computer operand |> pointer
    | 1UL -> bxl computer operand |> pointer
    | 2UL -> bst computer operand |> pointer
    | 3UL -> jnz computer operand
    | 4UL -> bxc computer         |> pointer
    | 5UL -> out computer operand |> pointer
    | 6UL -> bdv computer operand |> pointer
    | 7UL -> cdv computer operand |> pointer
    | u -> failwith $"Unexpected operator: {u}"
    
let rec run (memory : uint64[]) computer =
    if computer.Pointer > ((memory.Length - 1) |> uint64)
    then { computer with Output = computer.Output |> List.rev }
    else 
        let instruction = memory[computer.Pointer |> int]
        let operand = memory[(int computer.Pointer) + 1]
        run memory (applyInstruction computer instruction operand)
        
let mkComputer a b c p = { A = a; B = b; C = c; Pointer = p; Output = [] }

let parse (input:string) =
    let [|registers;program|] = input.Split("\n\n")
    let [|A;B;C|] =
        registers.Split("\n")
        |> Array.map (fun r -> r.Substring(12) |> uint64)
    let prog = program.Substring(9).Split(",") |> Array.map uint64
    mkComputer A B C 0UL, prog

let parseApply (memory:string) computer =
    let parsed = memory.Split(",") |> Array.map uint64
    computer |> run parsed

let runT () =
    printf "Testing.."
    //
    // test <@ Computer.create 64 3 0 |> run [|Adv |> toNum; 5|] = mkComputer 8 3 0 2 @>
    // test <@ Computer.create 5 1 0 |> run [|Adv |> toNum; 5|] = mkComputer 2 1 0 2 @>
    // test <@ Computer.create 0 4 0 |> run [|Bxl |> toNum; 2|] = mkComputer 0 6 0 2 @>
    // test <@ Computer.create 0 0 0 |> run [|Bst |> toNum; 3|] = mkComputer 0 3 0 2 @>
    // test <@ Computer.create 0 0 10 |> run [|Bst |> toNum; 6|] = mkComputer 0 2 10 2 @>
    // test <@ Computer.create 0 0 0 |> run [|Jnz |> toNum;0|] = mkComputer 0 0 0 2 @>
    // test <@ Computer.create 1 0 0 |> run [|Jnz |> toNum;7|] = mkComputer 1 0 0 7 @>
    // test <@ Computer.create 0 4 2 |> run [|Bxc |> toNum;7|] = mkComputer 0 6 2 2 @>
    // test <@ Computer.create 0 0 10 |> run [|Out |> toNum;6|] = { (mkComputer 0 0 10 2) with Output = [2] } @>
    // test <@ Computer.create 64 3 0 |> run [|Bdv |> toNum;5|] = mkComputer 64 8 0 2 @>
    // test <@ Computer.create 64 3 0 |> run [|Cdv |> toNum;5|] = mkComputer 64 3 8 2 @>
    //
    // test <@ Computer.create 0 0 9 |> parseApply "2,6" = mkComputer 0 1 9 2 @>
    // test <@ (Computer.create 10 0 0 |> parseApply "5,0,5,1,5,4").Output = [0;1;2] @>
    //
    // test <@ Computer.create 0 0 9 |> parseApply "2,6" = mkComputer 0 1 9 2 @>
    // test <@ (Computer.create 2024 0 0 |> parseApply "0,1,5,4,3,0").A = 0 @>
    // test <@ (Computer.create 2024 0 0 |> parseApply "0,1,5,4,3,0").Output = [4;2;5;6;7;7;7;7;3;1;0] @>
    // test <@ (Computer.create 0 29 0 |> parseApply "1,7").B = 26 @>
    // test <@ (Computer.create 0 2024 43690 |> parseApply "4,0").B = 44354 @>
    //
    // test <@ let c,m = example |> parse
    //         (run m c).Output = [5; 7; 3; 0] @>
    printfn "...done!"

runT ()


//35184372088832UL
//System.Math.Pow ((float 8), (float 15)) |> uint64
let c,m = parse input
let memory = m |> List.ofArray

// let result =
//     seq { for i in (Seq.initInfinite id) do
//               if i % 1_000_000 = 0 then printfn $"checking {i}"
//               if (run m { c with A = i }).Output = memory then yield i }
//     |> Seq.head

let rec solve acc c memory m =
    match m with
    | [] -> Some acc
    | h :: t ->
        let candidates =
            [0UL..7UL]
            |> List.map (fun cand -> acc * 8UL + cand)
            |> List.map (fun a -> a,(run memory { c with A = a }))
            |> List.filter (fun (a,c) -> c.Output.Length > 0 && c.Output[0] = h)
            |> List.map fst
        // printfn $"Acc: {acc} H: {h} T: {t} Candidates: %A{candidates}"
        candidates
        |> List.choose (fun a -> solve a c memory t)
        |> List.tryHead
            
solve 0UL c (Array.ofSeq memory) (memory |> Seq.rev |> List.ofSeq)
