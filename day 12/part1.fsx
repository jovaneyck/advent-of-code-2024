#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE""".Split("\n")
    |> Array.map _.Trim()
    |> List.ofSeq
let parse (input : string seq) = 
    [for rn, r in input |> Seq.indexed do
         for cn, c in r |> Seq.indexed ->
             (c,(rn,cn))]
    
    
let plantLocations = parse input
let locationsByPlants =
    plantLocations
    |> List.groupBy fst
    |> List.map (fun (plant, locs) -> plant, locs |> List.map snd)

let dist a b = abs (a - b)
let nextToEachOther one other =
    match one,other with
    | (r1,c1),(r2,c2) when r1 = r2 && dist c1 c2 = 1 -> true
    | (r1,c1),(r2,c2) when c1 = c2 && dist r1 r2 = 1 -> true
    | _ -> false

//Let's calculate regions recursively. 2 regions merge together if they touch each other orthogonally.
let rec merge regions =
    let mergeCandidate =
        regions
        |> Seq.indexed
        |> Seq.map (fun (i, region) ->
                    region,
                    regions
                    |> List.skip (i+1)
                    |> List.filter (fun other -> List.allPairs region other
                                                 |> List.exists (fun (a,b) -> nextToEachOther a b)))
        |> Seq.tryFind (fun (_, neighbourRegions) -> neighbourRegions.Length > 0)
    match mergeCandidate with
    | None -> regions
    | Some (region, mergeRegions) ->
        let toMerge = (region :: mergeRegions)
        let merged = toMerge |> List.collect id
        let rest = regions |> List.except toMerge
        merge (merged :: rest)

let regionsByPlants =
    locationsByPlants
    |> List.map (fun (plant, locations) ->
        plant,
        locations
        |> List.map List.singleton
        |> merge)

let area region = region |> Seq.length
let perimeter region =
    region
    |> Seq.map (fun location -> location, region |> List.filter (nextToEachOther location) |> List.length)
    |> Seq.map (fun (location, nbNeighbours) -> (location, 4 - nbNeighbours))
    |> Seq.sumBy snd

let result = 
    regionsByPlants
    |> List.map snd
    |> List.collect id
    |> List.sumBy (fun region -> (area region) * (perimeter region))

let run () =
    printf "Testing.."
    test <@ nextToEachOther (0,0) (1,0) @>
    test <@ nextToEachOther (0,0) (-1,0) @>
    test <@ nextToEachOther (0,0) (0,1) @>
    test <@ nextToEachOther (0,0) (0,-1) @>
    test <@ nextToEachOther (0,0) (1,1) |> not @>
    test <@ nextToEachOther (0,4) (6,3) |> not @>
    printfn "...done!"

run ()