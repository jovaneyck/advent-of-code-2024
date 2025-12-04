## F# Style Conventions (CRITICAL)

### On code comments
NEVER add any code comments unless explicitly requested. The code should be self-explanatory through clear naming and structure.

### Pure Functional Style - NO Mutations
- **NEVER use `mutable` keyword** unless explicitly requested
- **NEVER use `for` loops** - use `List.map`, `List.filter`, `List.fold`, `List.collect` instead
- Use immutable collections: `List`, `Set`, `Map` (prefer `List` as default)
- Example from `day 11/part2.fsx`: Custom `Counter` type wrapping `(Number * Count) list` for memoization

### Pipe-First Programming
Heavy use of pipeline operators (`|>`) over nested parentheses:
```fsharp
// Preferred style
input
|> List.map parseLine
|> List.filter isValid
|> List.collect expand
|> List.sum
```

### Pattern Matching Over Conditionals
Prefer `match` and `function` keyword over `if`:
```fsharp
let parseCommand =
    function
    | '^' -> U | '>' -> R
    | 'v' -> D | '<' -> L
    | unknown -> failwith $"unknown %c{unknown}"
```

### Naming Conventions
- **Descriptive collections**: `validUpdates`, `regionsByPlants`, `rulesToCheck` (plural forms)
- **Short names in pipelines**: `r`, `c` for row/column; `h`, `t` for head/tail
- **camelCase everywhere**: `maxRow`, `inBounds`, `nextToEachOther`
- **Modern string interpolation**: `$"text {value}"` not `sprintf`

### Type-Driven Design
Define custom types for domain clarity:
```fsharp
type Location = int * int
type Command = U | D | L | R
type State = { Robot: Location; Grid: Map<Location, Cell> }
```

## Testing with Unquote
All solutions use Unquote for assertions:
- Quotation syntax: `test <@ expression = expected @>`
- Standard test wrapper: `let run () = printf "Testing.."; test <@ ... @>; printfn "...done!"`
- Test with example data before running on full input
- Multiple test cases in sequence

## Data Parsing Patterns
Common parsing approaches from the codebase:
```fsharp
// Grid with coordinates
let parse input = 
    [for rn, r in input |> Seq.indexed do
         for cn, c in r |> Seq.indexed ->
             (c, (rn,cn))]

// Split and parse
let parseLine (line: string) =
    let [|result; operands|] = line.Split(": ")
    (int64 result, operands.Split(" ") |> Seq.map int64 |> Seq.toList)

// Windows line ending handling
let input = (System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt""").Replace("\r\n","\n")
```