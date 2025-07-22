#r "nuget: Unquote"
open Swensen.Unquote

let input =
    (System.IO.File.ReadAllText $"""day 09\input.txt""").Trim()

let example = "2333133121414131402"

let parseDiskMap (diskMap: string) =
    diskMap.ToCharArray()
    |> Array.map (fun c -> int c - int '0')
    |> Array.indexed
    |> Array.collect (fun (i, length) ->
        if i % 2 = 0 then
            Array.replicate length (Some (i / 2))
        else
            Array.replicate length None
    )

let compactDisk blocks =
    let mutable result = blocks
    let mutable fileIndex = Array.length result - 1
    
    for i in 0..Array.length result - 1 do
        if result.[i] = None then
            while fileIndex >= i && result.[fileIndex] = None do
                fileIndex <- fileIndex - 1
            
            if fileIndex >= i then
                result.[i] <- result.[fileIndex]
                result.[fileIndex] <- None
                fileIndex <- fileIndex - 1
    
    result

let calculateChecksum blocks =
    blocks
    |> Array.indexed
    |> Array.sumBy (fun (pos, fileId) ->
        match fileId with
        | Some id -> int64 pos * int64 id
        | None -> 0L
    )

let solve diskMap =
    diskMap
    |> parseDiskMap
    |> compactDisk
    |> calculateChecksum

let run () =
    printf "Testing.."
    test <@ solve example = 1928L @>
    test <@ solve input = 6367087064415L @>
    printfn "...done!"

run ()