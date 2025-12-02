let isMatch (input: string, chunks: int) = 
    if input.Length < chunks then
        false
    else 
        input
        |> Seq.chunkBySize ((input.Length + 1) / chunks)
        |> Seq.distinct
        |> Seq.length = 1

let rec isMatchRecursive (input: string, chunks: int) =
    if chunks > input.Length then
        false
    elif isMatch(input, chunks) then
        true
    else
        isMatchRecursive(input, chunks + 1)


let getMatchTotals (range: bigint list) =
    [range[0]..range[1]]
    |> Seq.map string
    |> Seq.filter (fun x -> isMatchRecursive(x, 2))
    |> Seq.map bigint.Parse
    |> Seq.sum

let readLines (filePath: string) = System.IO.File.ReadLines(filePath)

let parseInputRow (input: string) =
    input.Split [|','|]

let buildRanges (input: string) =
    input.Split [|'-'|]
    |> Seq.map bigint.Parse
    |> Seq.toList

let run path =
    readLines path
    |> Seq.take 1
    |> Seq.map parseInputRow
    |> Seq.concat
    |> Seq.map buildRanges
    |> Seq.map getMatchTotals
    |> Seq.sum
    
printfn "Input: %A" (run "input.txt")