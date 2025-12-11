let findPaths (map: Map<string, string array>) (from: string) (to': string) =
    let mutable processed = Map.empty<string, int64>

    let rec findOut (current: string) (acc: int64) =
        // If we are 'out' add it to the acc
        if current = to' then
            acc + 1L
        else if current = "out" then
            acc
        else if processed.ContainsKey current then
            processed[current]
        else
            let mutable localAcc = 0L

            // Get the current node
            let currentNode = map[current]

            // Outputs
            for output in currentNode do
                let result = (findOut output localAcc)
                processed <- processed.Add (output, result)

                localAcc <- localAcc + result

            localAcc

    findOut from 0

let run filePath =
    let map =
        System.IO.File.ReadLines filePath
        |> Seq.map(fun line ->
            let parts = line.Split ": "
            let source = parts[0]
            let outputs = parts[1].Split ' '

            (source, outputs)
        )
        |> Map
    
    // svr->dac->fft->out
    let svr_dac = findPaths map "svr" "dac"
    let dac_fft = findPaths map "dac" "fft"
    let fft_out = findPaths map "fft" "out"
    let total1 = svr_dac * dac_fft * fft_out

    // svr->fft->dac->out
    let svr_fft = findPaths map "svr" "fft"
    let fft_dac = findPaths map "fft" "dac"
    let dac_out = findPaths map "dac" "out"
    let total2 = svr_fft * fft_dac * dac_out

    total1 + total2

printfn "Result: %A" (run "input.txt")