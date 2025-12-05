let parseInput filePath separator =
    let lines = System.IO.File.ReadAllLines filePath
    let separatorIdx = Seq.findIndex (fun x -> x = "") lines
    let freshIDRanges = lines |> Seq.take separatorIdx
    let availableIDs = lines |> Seq.skip (separatorIdx + 1) |> Seq.map int64

    (freshIDRanges, availableIDs)

let processRanges (input: string seq) =
    input
    |> Seq.map (fun range -> range.Split '-')
    |> Seq.map (fun range -> 
        match range with
        | [| start; finish |] -> 
            ((start |> int64), (finish |> int64))
        | _ -> failwith "Could not parse range"
    )
    |> Seq.toList

let combineRanges ranges =
    let rec combine (ranges: (int64 * int64) list) idx =
        if idx = (List.length ranges) - 1 then
            ranges
        else
            let (range_0_start, range_0_finish) = ranges[idx]
            let (range_1_start, range_1_finish) = ranges[idx + 1]

            if range_0_finish >= range_1_start then
                let new_range_start = range_0_start
                let new_range_finish = max range_0_finish range_1_finish

                combine (ranges[..idx-1] @ (new_range_start, new_range_finish) :: ranges[idx+2..]) idx 
            else
                combine ranges (idx + 1)

    combine ranges 0

let run filePath =
    let (freshIDRanges, _) = parseInput filePath ""
    let ranges = processRanges freshIDRanges

    ranges
    |> List.sort
    |> combineRanges
    |> Seq.map (fun (start, finish) -> (finish + 1L) - start)
    |> Seq.sum


    
printfn "Result: %A" (run "input.txt")