let findPaths (map: Map<string, string array>) =
    let rec findOut (current: string) (acc: int) =
        printfn "Current: %s, Acc: %d" current acc

        // If we are 'out' add it to the acc
        if current = "out" then
            acc + 1
        else
            let mutable localAcc = 0

            // Get the current node
            let currentNode = map[current]

            // Outputs
            for output in currentNode do
                localAcc <- localAcc + (findOut output localAcc)

            localAcc

    findOut "you" 0

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
    
    findPaths map


printfn "Result: %A" (run "input.txt")