#r "nuget: Unquote"
open Swensen.Unquote

type Coord = int * int
type Frequency = char

let example = 
    """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............""".Split('\n') |> Array.map (_.Trim()) |> List.ofArray

let input =
    System.IO.File.ReadAllLines $"""day 08/input.txt"""
    |> List.ofArray

let parseGrid lines =
    lines
    |> List.mapi (fun r line -> 
        line |> Seq.mapi (fun c ch -> (r,c), ch))
    |> List.collect List.ofSeq
    |> List.filter (fun (_, ch) -> ch <> '.')
    |> List.map (fun (coord, freq) -> freq, coord)
    |> List.groupBy fst
    |> List.map (fun (freq, coords) -> freq, coords |> List.map snd)

let inBounds (maxR, maxC) (r, c) =
    r >= 0 && r <= maxR && c >= 0 && c <= maxC

let findAntinodesOnLine bounds (r1,c1) (r2,c2) =
    let dr = r2 - r1
    let dc = c2 - c1
    
    let rec extend direction (r,c) acc =
        let nr = r + direction * dr
        let nc = c + direction * dc
        if inBounds bounds (nr, nc) then
            extend direction (nr, nc) ((nr, nc) :: acc)
        else acc
    
    let forward = extend 1 (r1,c1) [(r1,c1); (r2,c2)]
    let backward = extend (-1) (r1,c1) []
    forward @ backward

let findAntinodes bounds antennas =
    antennas
    |> List.collect (fun (freq, coords) ->
        if List.length coords < 2 then []
        else
            coords
            |> List.allPairs coords
            |> List.filter (fun (a, b) -> a <> b)
            |> List.collect (fun (a, b) -> findAntinodesOnLine bounds a b)
            |> List.distinct)
    |> Set.ofList

let solve lines =
    let maxR = List.length lines - 1
    let maxC = String.length lines[0] - 1
    let bounds = (maxR, maxC)
    
    lines
    |> parseGrid
    |> findAntinodes bounds
    |> Set.count

let run () =
    printf "Testing.."
    test <@ solve example = 34 @>
    test <@ solve input |> fun x -> x > 0 @>
    printfn "...done!"
    
    printfn $"Part 2: {solve input}"

run ()