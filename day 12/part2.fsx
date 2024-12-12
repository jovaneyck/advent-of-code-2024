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

//For bulk discounts we need to count the number of sides instead of the total perimeter
//Number of sides == number of "corners" on a region's perimeter
let nbCorners region location =
    let exists loc = region |> Seq.contains loc
    let r,c = location
    
    [//outer corners
        // upper right
        if (r,c+1) |> exists |> not && (r-1, c) |> exists |> not then 1 else 0
        // lower right
        if (r,c+1) |> exists |> not && (r+1, c) |> exists |> not then 1 else 0
        //upper left
        if (r,c-1) |> exists |> not && (r-1, c) |> exists |> not then 1 else 0
        //lower left
        if (r,c-1) |> exists |> not && (r+1, c) |> exists |> not then 1 else 0
     //inner corners
        if (r,c+1) |> exists && (r+1,c) |> exists && (r+1,c+1) |> exists |> not then 1 else 0 
        if (r,c-1) |> exists && (r+1,c) |> exists && (r+1,c-1) |> exists |> not then 1 else 0
        if (r,c+1) |> exists && (r-1,c) |> exists && (r-1,c+1) |> exists |> not then 1 else 0 
        if (r,c-1) |> exists && (r-1,c) |> exists && (r-1,c-1) |> exists |> not then 1 else 0 
    ]
    |> List.sum
   
let nbSides region = 
    region
    |> List.map (nbCorners region)
    |> List.sum

let result = 
    regionsByPlants
    |> List.map snd
    |> List.collect id
    |> List.sumBy (fun region -> (area region) * (nbSides region))

let run () =
    printf "Testing.."
    test <@ nextToEachOther (0,0) (1,0) @>
    test <@ nextToEachOther (0,0) (-1,0) @>
    test <@ nextToEachOther (0,0) (0,1) @>
    test <@ nextToEachOther (0,0) (0,-1) @>
    test <@ nextToEachOther (0,0) (1,1) |> not @>
    test <@ nextToEachOther (0,4) (6,3) |> not @>
    
    let e_example = [(0, 0); (0, 1); (1, 0); (0, 2); (2, 0); (0, 3); (2, 1); (3, 0); (0, 4); (2, 2); (4, 0); (2, 3); (4, 1); (2, 4); (4, 2); (4, 3); (4, 4)]
    test <@ nbSides e_example = 12 @>

    printfn "...done!"

run ()