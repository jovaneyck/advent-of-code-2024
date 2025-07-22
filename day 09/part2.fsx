#r "nuget: Unquote"
open Swensen.Unquote

let input =
    (System.IO.File.ReadAllText $"""day 09\input.txt""").Trim()

let example = "2333133121414131402"

type Block = { FileId: int option; Size: int }

let parseDiskMapPart2 (diskMap: string) =
    diskMap.ToCharArray()
    |> Array.map (fun c -> int c - int '0')
    |> Array.indexed
    |> Array.map (fun (i, length) ->
        if i % 2 = 0 then
            { FileId = Some (i / 2); Size = length }
        else
            { FileId = None; Size = length }
    )
    |> Array.filter (fun block -> block.Size > 0)

let findLeftmostFreeSpace (blocks: Block[]) fileSize filePos =
    let mutable foundPos = -1
    
    for i in 0..filePos - 1 do
        if blocks.[i].FileId = None && blocks.[i].Size >= fileSize && foundPos = -1 then
            foundPos <- i
    
    if foundPos >= 0 then Some foundPos else None

let compactWholeFiles blocks =
    let mutable result = Array.copy blocks
    let maxFileId = 
        result 
        |> Array.choose (fun b -> b.FileId) 
        |> Array.max
    
    for fileId in maxFileId .. -1 .. 0 do
        let filePos = 
            result 
            |> Array.findIndex (fun b -> b.FileId = Some fileId)
        
        let fileSize = result.[filePos].Size
        
        match findLeftmostFreeSpace result fileSize filePos with
        | Some freePos ->
            let freeSize = result.[freePos].Size
            
            result.[freePos] <- { FileId = Some fileId; Size = fileSize }
            result.[filePos] <- { FileId = None; Size = fileSize }
            
            if freeSize > fileSize then
                result <- 
                    Array.concat [
                        result.[0..freePos]
                        [| { FileId = None; Size = freeSize - fileSize } |]
                        result.[freePos + 1..]
                    ]
        | None -> ()
    
    result

let blocksToArray blocks =
    blocks
    |> Array.collect (fun block ->
        Array.replicate block.Size block.FileId
    )

let calculateChecksum blocks =
    blocks
    |> Array.indexed
    |> Array.sumBy (fun (pos, fileId) ->
        match fileId with
        | Some id -> int64 pos * int64 id
        | None -> 0L
    )

let solvePart2 diskMap =
    diskMap
    |> parseDiskMapPart2
    |> compactWholeFiles
    |> blocksToArray
    |> calculateChecksum

let run () =
    printf "Testing.."
    test <@ solvePart2 example = 2858L @>
    test <@ solvePart2 input = 6390781891880L @>
    printfn "...done!"

run ()
