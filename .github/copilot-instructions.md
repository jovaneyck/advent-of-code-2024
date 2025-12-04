# Advent of Code 2024 - F# Solutions

## Project Overview
F# Script (.fsx) solutions for Advent of Code 2024 puzzles, designed for interactive execution in F# Interactive (FSI). Each daily puzzle has its own directory (`day 01/`) containing `input.txt`, `part1.fsx`, and `part2.fsx` files.

## Code Execution Model
All solutions are **F# scripts (.fsx)** executed directly in FSI, not compiled projects:
- Run with `dotnet fsi part1.fsx` in PowerShell or use fsi-mcp server if available.
- Use `__SOURCE_DIRECTORY__` for relative file paths to input data
- Files are self-contained with inline NuGet references
- Performance timing via `#time` directive

## Standard File Structure
Every solution file follows this pattern:
```fsharp
#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt""" |> List.ofSeq

let example = """sample data""".Split("\n") |> Array.map (fun s -> s.Trim()) |> List.ofSeq

// Solution code here

let run () =
    printf "Testing.."
    test <@ example |> parse |> solve = expectedResult @>
    printfn "...done!"

run ()
```

## Coding style Guidelines

Read and apply the coding style from [coding style.prompt.md](../prompts/coding-style.prompt.md):

## Key Files
- `day 00`: Folder containing template file for new solutions

