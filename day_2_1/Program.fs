let isMatch (input: string) = input[..(input.Length/2)-1] = input[(input.Length/2)..]

let getMatchTotals (range: bigint list) =
    [range[0]..range[1]]
    |> Seq.map string
    |> Seq.filter isMatch
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