#r "nuget: Unquote"
open Swensen.Unquote


let input =
    (System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt""").Replace("\r\n","\n")

let example =
    """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""

let POINTER_STEP_SIZE = 2

type Computer = { A : int; B: int; C: int; Pointer: int; Output : int list }

module Computer =
    let create a b c = { A = a; B = b; C = c; Pointer = 0; Output = [] }

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
    | Adv -> 0
    | Bxl -> 1
    | Bst -> 2
    | Jnz -> 3
    | Bxc -> 4
    | Out -> 5
    | Bdv -> 6
    | Cdv -> 7
    
let valueOfComboOperand computer =
    function
        | 0 -> 0
        | 1 -> 1
        | 2 -> 2
        | 3 -> 3
        | 4 -> computer.A
        | 5 -> computer.B
        | 6 -> computer.C
        | unexpected -> failwith $"Unexpected combo operand {unexpected}"
        
let adv computer operand =
    let nom = computer.A
    let denom = pown 2 (valueOfComboOperand computer operand)
    let result = nom / denom
    { computer with A = result }
    
let bdv computer operand =
    let nom = computer.A
    let denom = pown 2 (valueOfComboOperand computer operand)
    let result = nom / denom
    { computer with B = result }
    
let cdv computer operand =
    let nom = computer.A
    let denom = pown 2 (valueOfComboOperand computer operand)
    let result = nom / denom
    { computer with C = result }
        
let bxl computer operand =
    let result = computer.B ^^^ operand
    { computer with B = result }
    
let bst computer operand =
    let result = (valueOfComboOperand computer operand) % 8
    { computer with B = result }
    
let jnz computer operand =
    if computer.A = 0
    then {computer with Pointer = computer.Pointer + POINTER_STEP_SIZE}
    else { computer with Pointer = operand }

let bxc computer =
    let result = computer.B ^^^ computer.C
    { computer with B = result }

let out computer operand =
    let result = (valueOfComboOperand computer operand) % 8
    { computer with Output = result :: computer.Output }

let applyInstruction computer operator operand =
    let pointer computer = { computer with Pointer = computer.Pointer + POINTER_STEP_SIZE }
    match operator with
    | 0 -> adv computer operand |> pointer
    | 1 -> bxl computer operand |> pointer
    | 2 -> bst computer operand |> pointer
    | 3 -> jnz computer operand
    | 4 -> bxc computer         |> pointer
    | 5 -> out computer operand |> pointer
    | 6 -> bdv computer operand |> pointer
    | 7 -> cdv computer operand |> pointer
    | u -> failwith $"Unexpected operator: {u}"
    
let rec run (memory : int[]) computer =
    if computer.Pointer > (memory.Length - 1)
    then { computer with Output = computer.Output |> List.rev }
    else 
        let instruction = memory[computer.Pointer]
        let operand = memory[computer.Pointer + 1]
        run memory (applyInstruction computer instruction operand)
        
let mkComputer a b c p = { A = a; B = b; C = c; Pointer = p; Output = [] }

let parse (input:string) =
    let [|registers;program|] = input.Split("\n\n")
    let [|A;B;C|] =
        registers.Split("\n")
        |> Array.map (fun r -> r.Substring(12) |> int)
    let prog = program.Substring(9).Split(",") |> Array.map int
    mkComputer A B C 0, prog

let parseApply (memory:string) computer =
    let parsed = memory.Split(",") |> Array.map int
    computer |> run parsed

let runT () =
    printf "Testing.."
    
    test <@ Computer.create 64 3 0 |> run [|Adv |> toNum; 5|] = mkComputer 8 3 0 2 @>
    test <@ Computer.create 5 1 0 |> run [|Adv |> toNum; 5|] = mkComputer 2 1 0 2 @>
    test <@ Computer.create 0 4 0 |> run [|Bxl |> toNum; 2|] = mkComputer 0 6 0 2 @>
    test <@ Computer.create 0 0 0 |> run [|Bst |> toNum; 3|] = mkComputer 0 3 0 2 @>
    test <@ Computer.create 0 0 10 |> run [|Bst |> toNum; 6|] = mkComputer 0 2 10 2 @>
    test <@ Computer.create 0 0 0 |> run [|Jnz |> toNum;0|] = mkComputer 0 0 0 2 @>
    test <@ Computer.create 1 0 0 |> run [|Jnz |> toNum;7|] = mkComputer 1 0 0 7 @>
    test <@ Computer.create 0 4 2 |> run [|Bxc |> toNum;7|] = mkComputer 0 6 2 2 @>
    test <@ Computer.create 0 0 10 |> run [|Out |> toNum;6|] = { (mkComputer 0 0 10 2) with Output = [2] } @>
    test <@ Computer.create 64 3 0 |> run [|Bdv |> toNum;5|] = mkComputer 64 8 0 2 @>
    test <@ Computer.create 64 3 0 |> run [|Cdv |> toNum;5|] = mkComputer 64 3 8 2 @>
    
    test <@ Computer.create 0 0 9 |> parseApply "2,6" = mkComputer 0 1 9 2 @>
    test <@ (Computer.create 10 0 0 |> parseApply "5,0,5,1,5,4").Output = [0;1;2] @>
    
    test <@ Computer.create 0 0 9 |> parseApply "2,6" = mkComputer 0 1 9 2 @>
    test <@ (Computer.create 2024 0 0 |> parseApply "0,1,5,4,3,0").A = 0 @>
    test <@ (Computer.create 2024 0 0 |> parseApply "0,1,5,4,3,0").Output = [4;2;5;6;7;7;7;7;3;1;0] @>
    test <@ (Computer.create 0 29 0 |> parseApply "1,7").B = 26 @>
    test <@ (Computer.create 0 2024 43690 |> parseApply "4,0").B = 44354 @>
    
    test <@ let c,m = example |> parse
            (run m c).Output = [4; 6; 3; 5; 6; 3; 5; 2; 1; 0] @>
    printfn "...done!"

runT ()

let c,m = parse input
(run m c).Output |> List.map string |> String.concat ","