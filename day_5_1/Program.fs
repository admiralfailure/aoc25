let parseInput filePath separator =
    let lines = System.IO.File.ReadAllLines filePath
    let separatorIdx = Seq.findIndex (fun x -> x = "") lines
    let freshIDRanges = lines |> Seq.take separatorIdx
    let availableIDs = lines |> Seq.skip (separatorIdx + 1) |> Seq.map int64

    (freshIDRanges, availableIDs)

let isWithinRange start finish value =
    value >= start && value <= finish

let processRanges (input: string seq) =
    input
    |> Seq.map (fun range -> range.Split '-')
    |> Seq.map (fun range -> 
        match range with
        | [| start; finish |] -> (isWithinRange (start |> int64) (finish |> int64))
        | _ -> failwith "Could not parse start and finish from range"
    )

let run filePath =
    let (freshIDRanges, availableIDs) = parseInput filePath ""
    let rangeFunctions = processRanges freshIDRanges

    availableIDs
    |> Seq.map (fun id ->
        rangeFunctions
        |> Seq.exists (fun rangeFunction -> rangeFunction id)
    )
    |> Seq.filter (fun result -> result)
    |> Seq.length

    
printfn "Result: %A" (run "input.txt")