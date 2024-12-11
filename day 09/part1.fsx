#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""

let example = "2333133121414131402"
    
let run () =
    printf "Testing.."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()