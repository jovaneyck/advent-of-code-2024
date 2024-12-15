#r "nuget: Unquote"
open Swensen.Unquote

let input =
   (System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt""")
       .Replace("\r\n","\n")

let example =
    """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""
let small_example = """#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"""

//Array2D has O(1) access which would be optimal but has no efficient copy-on-write.
//I'm sticking with an immutable Map<int*int,Cell> for today.

type Command = U | D | L | R
type Cell = Wall | Empty | LBox | RBox //we keep track of the robot location outside the grid
type Location = int * int //row,column
type Grid = Map<Location, Cell>
type State = { Robot : Location; Grid: Grid }

let expand (input : string) =
    input
        .Replace("#", "##")
        .Replace("O", "[]")
        .Replace(".", "..")
        .Replace("@", "@.")

let parseState (input : string) =
    let expanded = input |> expand
    let grid : Grid =
        [for rn, row in expanded |> _.Split("\n") |> Seq.indexed do
             for cn, cell in row |> Seq.indexed do
                 let contents = match cell with
                                 | '#' -> Wall
                                 | '.' -> Empty
                                 | '@' -> Empty
                                 | '[' -> LBox
                                 | ']' -> RBox
                                 | unknown -> failwith $"unknown character %c{unknown}"
                 yield (rn,cn), contents
        ]
        |> Map.ofSeq
    
    let robotLocation = 
        [for rn, row in expanded.Split("\n") |> Seq.indexed do
             for cn, cell in row |> Seq.indexed do
                 if cell = '@' then yield (rn,cn)
        ] |> Seq.head
    { Robot = robotLocation; Grid = grid }

let parseCommand =
    function
    | '^' -> U
    | '>' -> R
    | 'v' -> D
    | '<' -> L

let parse (input : string) =
    let [|grid; commands|] = input.Split("\n\n")
    let state = parseState grid
    let commands = commands.Split("\n") |> Seq.collect id |> Seq.map parseCommand |> List.ofSeq
    state,commands

let delta =
    function
    | U -> (-1,0)
    | D -> (1,0)
    | L -> (0,-1)
    | R -> (0,1)
    
let move command (r,c) =
    let (dr, dc) = delta command
    (r+dr,c+dc)

let at location grid  = grid |> Map.find location

let rec tryPushLeft state location =
    let left = location |> move L
    if (state.Grid |> at left <> LBox) then failwith $"Expected an LBox, but found something else" //just making sure
    let leftOfBox = left |> move L
    let update = (left, location),(leftOfBox,left)
    match state.Grid |> at leftOfBox with
    | Wall -> None
    | Empty -> Some [update] 
    | RBox ->
        let sub = tryPushLeft state leftOfBox
        match sub with
        | None -> None
        | Some updates -> Some (update :: updates)
    | problem -> failwith $"Did not expect to encounter a <{problem}> right about here!"

let rec tryPushRight state location =
    let right = location |> move R
    if (state.Grid |> at right <> RBox) then failwith $"Expected an RBox, but found something else"
    let righttOfBox = right |> move R
    let update = (location, right),(right,righttOfBox)
    match state.Grid |> at righttOfBox with
    | Wall -> None
    | Empty -> Some [update] 
    | LBox ->
        let sub = tryPushRight state righttOfBox
        match sub with
        | None -> None
        | Some updates -> Some (update :: updates)
    | problem -> failwith $"Did not expect to encounter a <{problem}> right about here!"

let rec tryPushDown state location =
    let lr,lc = location
    let atLocation = state.Grid |> at location
    if atLocation = Empty then Some []
    else 
        let leftLocation, rightLocation =
            match atLocation with
            | LBox -> location, (lr, lc+1)
            | RBox -> (lr,lc-1),location
            | problem -> failwith $"Was expecting box only stuff here, but found: <{problem}>"
        
        let actual = (state.Grid |> at leftLocation, state.Grid |> at rightLocation)
        if ((LBox, RBox) <> actual)
        then failwith $"Expected an (LBox,RBox) but found something else: {actual}"
        
        let underBoxLeft,underBoxRight = leftLocation |> move D, rightLocation |> move D
        let update = (leftLocation, rightLocation),(underBoxLeft,underBoxRight)
        match state.Grid |> at underBoxLeft, state.Grid |> at underBoxRight with
        | Wall, _ -> None
        | _, Wall -> None
        | Empty,Empty -> Some [update] 
        | _ -> //Boxes somewhere
            let subOne = tryPushDown state underBoxLeft
            let subOther = tryPushDown state underBoxRight
            match subOne, subOther with
            | None,_ -> None
            | _,None -> None
            | Some updates, Some otherUpdates -> Some (update :: updates @ otherUpdates)
        
let rec tryPushUp state location =
    let lr,lc = location
    let atLocation = state.Grid |> at location
    if atLocation = Empty then Some []
    else 
        let leftLocation, rightLocation =
            match state.Grid |> at location with
            | LBox -> location, (lr, lc+1)
            | RBox -> (lr,lc-1),location
            | problem -> failwith $"Was expecting box only stuff here, but found: <{problem}>"
            
        
        let actual = (state.Grid |> at leftLocation, state.Grid |> at rightLocation)
        if ((LBox, RBox) <> actual)
        then failwith $"Expected an (LBox,RBox) but found something else: {actual}"
        
        let aboveBoxLeft,aboveBoxRight = leftLocation |> move U, rightLocation |> move U
        let update = (leftLocation, rightLocation),(aboveBoxLeft,aboveBoxRight)
        match state.Grid |> at aboveBoxLeft, state.Grid |> at aboveBoxRight with
        | Wall, _ -> None
        | _, Wall -> None
        | Empty,Empty -> Some [update] 
        | _ -> //Boxes somewhere
            let subOne = tryPushUp state aboveBoxLeft
            let subOther = tryPushUp state aboveBoxRight
            match subOne, subOther with
            | None,_ -> None
            | _,None -> None
            | Some updates, Some otherUpdates -> Some (update :: updates @ otherUpdates)

//We'll try to push boxes recursively. If all works out we apply the updates in Some,
// otherwise we hit something and we do None.
let tryPush state location command =
    //important to do all the updates at once instead of one by one or you might override some temp states incorrectly
    let applyUpdates state updates =
        let olds = updates |> List.map fst
        let news = updates |> List.map snd
        let cleaned = olds |> List.fold (fun state (oldl, oldr) -> state |> Map.add oldl Empty |> Map.add oldr Empty) state
        news |> List.fold (fun state (newl,newr) -> state |> Map.add newl LBox |> Map.add newr RBox) cleaned
    
    let updates = 
        match command with
        | L -> tryPushLeft state location
        | R -> tryPushRight state location
        | D -> tryPushDown state location
        | U -> tryPushUp state location
    // printfn $"Processing push {command}.."
    match updates with
    | None -> state
    | Some updates ->
        // printfn $"We have {updates.Length} updates: {updates}"
        let updatedGrid = applyUpdates state.Grid (updates|> List.rev) //much important, start updating from the "far end"
        { state with Robot = location; Grid = updatedGrid }
    

let apply state command =
    // printfn $"Processing command {command} at {state.Robot}.."
    let next = state.Robot |> move command
    let atNext = state.Grid |> at next
    
    match atNext with
    | Wall -> state
    | Empty -> { state with Robot = next }
    | LBox -> tryPush state next command
    | RBox -> tryPush state next command

let applyAll state commands = commands |> List.fold apply state

let coord (r,c) = 100*r+c

let solve input =
    let state, commands = parse input
    let final = applyAll state commands
    let result =
        final.Grid
        |> Map.filter (fun _ el -> el.IsLBox)
        |> Map.keys
        |> Seq.sumBy coord
    result

let toString state =
    let rMax = state.Grid |> Map.keys |> Seq.map fst |> Seq.max
    let cMax = state.Grid |> Map.keys |> Seq.map snd |> Seq.max
    [for r in 0..rMax do
         [for c in 0..cMax do
             if (r,c) = state.Robot then "@"
             else match state.Grid |> at (r,c) with
                  | Wall ->  "#"
                  | Empty -> "."
                  | LBox ->  "["
                  | RBox ->  "]"]
         |>String.concat ""
    ] |> String.concat "\n"

let print state =
    state |> toString |> printfn "%s"

let testPush example =
        let state, commands = parse example
        let result = applyAll state commands
        result |> toString

let run () =
    printf "Testing.."
    
    let left_example = """#####
#.O@#
#####

<<"""    
    
    test <@ (testPush left_example) = """##########
##[]@...##
##########""" @>
    
    let right_example = """#####
#@O.#
#####

>>>>"""

    let harder_left_example = """#######
#.O.O@#
#######

<<<<<"""
    test <@  testPush harder_left_example = """##############
##[][]@.....##
##############""" @>

    
    test <@ (testPush right_example) = """##########
##...@[]##
##########""" @>
    
    let harder_right_example = """#######
#@O.O.#
#######

>>>>>>"""

    test <@ testPush harder_right_example = """##############
##.....@[][]##
##############""" @>
    
    let down_example = """###
#@#
#O#
#.#
###

vv"""

    test <@  testPush down_example = """######
##..##
##@.##
##[]##
######""" @>

    
    let tilted_down_example = """###
#@#
#O#
#.#
###

>vv"""

    test <@  testPush tilted_down_example = """######
##..##
##.@##
##[]##
######""" @>
      
    let harder_down_example = """###
#@#
#O#
#.#
#O#
#.#
###

v>vv"""

    test <@ testPush harder_down_example = """######
##..##
##..##
##.@##
##[]##
##[]##
######"""  @>
    
    let down_pyramid_example = """#####
#.@.#
#.O.#
#.OO#
#...#
#####

<v>^>>vv"""

    test <@ testPush down_pyramid_example = """##########
##......##
##....@.##
##...[].##
##..[][]##
##########""" @>
    
    
    let up_example = """###
#.#
#O#
#@#
###

^^"""
    test <@ testPush up_example = """######
##[]##
##@.##
##..##
######""" @>

    let tilted_up_example = """###
#.#
#O#
#@#
###

>^^"""
    test <@ testPush tilted_up_example = """######
##[]##
##.@##
##..##
######""" @>

    
    let harder_up_example = """###
#.#
#O#
#.#
#O#
#@#
###

^^^"""

    test <@ testPush harder_up_example = """######
##[]##
##[]##
##@.##
##..##
##..##
######""" @>
    
    let up_pyramid_example = """#####
#...#
#.OO#
#.O.#
#.@.#
#####

<^>v>^^"""

    test <@ testPush up_pyramid_example = """##########
##..[][]##
##...[].##
##...@..##
##......##
##########""" @>
    
    
    let shaky_stack_example = """#####
#...#
#.O.#
#.O.#
#.O.#
#.@.#
#####

<^><^^><vvv>>>^"""

    test <@  testPush shaky_stack_example = """##########
##...[].##
##..[]..##
##...[].##
##....@.##
##......##
##########""" @>

    let dense_pyramid_example = """######
#....#
#.O..#
#@OO.#
#.OOO#
#....#
######

>><^>>^>>v"""
    test <@ testPush dense_pyramid_example = "############
##........##
##.....@..##
##....[]..##
##...[][].##
##..[][][]##
############" @>
        
    test <@ solve small_example = 618 @>
    test <@ solve example = 9021 @>    
    printfn "...done!"

run ()

solve input