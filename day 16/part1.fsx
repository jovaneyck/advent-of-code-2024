#r "nuget: Unquote"
open Swensen.Unquote
#r "nuget: FSharpx.Collections"
open FSharpx.Collections

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """dd"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

///Dijkstra Priority Queue. Combines a (priority) Heap with a visited Set so we can update priorities of a node without having to iterate over the entire heap
module DPQ =
    type State<'t> when 't: comparison =
        { Heap: Heap<int * 't>
          Visited: Set<'t>
          Distances: Map<'t, int> }

    let private heapOf s = Heap.ofSeq false s

    let ofSeq s =
        { Heap = heapOf s
          Visited = Set.empty
          Distances = Map.empty }

    let rec tryUncons pq =
        pq.Heap
        |> Heap.tryUncons
        |> Option.bind (fun ((d, h), t) ->
            if pq.Visited |> Set.contains h then
                tryUncons { pq with Heap = t }
            else
                ((d, h),
                 { Visited = pq.Visited |> Set.add h
                   Distances = pq.Distances |> Map.add h d
                   Heap = t })
                |> Some)

    let visited x pq = pq.Visited |> Set.contains x

    let updateDistances updates pq =
        let unvisited =
            updates
            |> List.filter (fun n -> pq |> visited (snd n) |> not)

        { pq with Heap = pq.Heap |> Heap.merge (heapOf unvisited) }


///Immutable version of Dijkstra's shortest path algorithm
///https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
let rec dijkstra successors cost (pq: DPQ.State<'a>) =
    match pq |> DPQ.tryUncons with
    | None -> pq
    | Some ((dist, coord), pqrest) ->
        let neighbours = successors coord
        let costed = neighbours |> List.map (fun n -> (cost dist n, n))
        let nextpq = pqrest |> DPQ.updateDistances costed

        dijkstra successors cost nextpq