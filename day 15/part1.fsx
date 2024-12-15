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
let small_example = """########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"""

//Array2D has O(1) access which would be optimal but has no efficient copy-on-write.
//I'm sticking with an immutable Map<int*int,Cell> for today.

type Command = U | D | L | R
type Cell = Wall | Empty | Box //we keep track of the robot location outside the grid
type Location = int * int //row,column
type Grid = Map<Location, Cell>
type State = { Robot : Location; Grid: Grid }

let parseState (input : string) =
    let grid : Grid = 
        [for rn, row in input.Split("\n") |> Seq.indexed do
             for cn, cell in row |> Seq.indexed do
                 let contents = match cell with
                                 | '#' -> Wall
                                 | '.' -> Empty
                                 | '@' -> Empty
                                 | 'O' -> Box
                                 | unknown -> failwith $"unknown character %c{unknown}"
                 yield (rn,cn), contents
        ]
        |> Map.ofSeq
    
    let robotLocation = 
        [for rn, row in input.Split("\n") |> Seq.indexed do
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

let push state command =
    let dr,dc = delta command
    let r,c = state.Robot
    let ray = Seq.initInfinite (fun i -> (r + (i+1)*dr, c + (i+1)*dc))
    let firstNonBoxLoc, firstNonBoxEl =
        ray
        |> Seq.map(fun loc -> (loc, state.Grid |> at loc))
        |> Seq.find(fun (_,el) -> el.IsEmpty || el.IsWall)
    match firstNonBoxEl with
    | Wall -> //continuous row of boxes against a wall, NOOP
        state
    | Empty -> //continuous row of boxes with an empty hole, PUUUSH
        //robot moves 1 space and all boxes move along. We just teleport the first box to the final spot
        let nextRobot = state.Robot |> move command
        let nextGrid =
            state.Grid
            |> Map.add nextRobot Empty
            |> Map.add firstNonBoxLoc Box
        { state with Robot = nextRobot; Grid = nextGrid }
    | Box -> failwith "Should never happen as we're putting all boxes in the ray"
    
let apply state command =
    let next = state.Robot |> move command
    let atNext = state.Grid |> at next
    match atNext with
    | Wall -> state
    | Empty -> { state with Robot = next }
    | Box -> push state command

let coord (r,c) = 100*r+c

let solve input =
    let state, commands = parse input
    let final = commands |> List.fold apply state
    let result =
        final.Grid
        |> Map.filter (fun _ el -> el.IsBox)
        |> Map.keys
        |> Seq.sumBy coord
    result

let run () =
    printf "Testing.."
    test <@ solve small_example = 2028 @>
    test <@ solve example = 10092 @>
    test <@ solve input = 1515788 @>
    printfn "...done!"

run ()