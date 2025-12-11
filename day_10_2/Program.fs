type Machine = { Counters: int array; Buttons: int array array }

let parseInputLine (line: string) =
    let parts = line.Split ' '

    //printfn "Split into parts: %A" parts

    let indicatorPart = (parts[0])[1..parts[0].Length - 2]
    //printfn "Indicator part: %A" indicatorPart

    let counterPart = (parts[parts.Length-1])[1..parts[parts.Length-1].Length - 2]
    let counters =
        counterPart.Split ','
        |> Array.map int

    let buttons = 
        parts[1..parts.Length - 2]
        |> Array.map(fun buttonList -> 
            //printfn "Processing %A" buttonList

            // Convert the list to a binary string
            let ones = 
                buttonList[1..buttonList.Length - 2].Split ',' 
                |> Array.map int

            //printfn "Ones: %A" ones

            let binary = 
                [0..counters.Length-1]
                |> Seq.map(fun idx ->
                    //printfn "Checking idx %A" idx
                    if Array.contains idx ones then
                        1
                    else
                        0
                )
                |> Seq.toArray

            //printfn "Binary: %A" binary

            binary
        )
        |> Array.sortBy(fun button -> -Array.sum button)

    { Counters = counters; Buttons = buttons }

let subtractArrays (source: int array) (modifier: int array) =
    if source.Length <> modifier.Length then
        failwith "Unable to subtract arrays of mixed length"
    else
        let result = 
            source
            |> Array.mapi(fun idx value ->
                value - modifier[idx]
            )

        if Array.exists(fun x -> x < 0) result then
            (false, source)
        else
            (true, result)

let countButtonPresses machine =
    printfn "Processing machine %A..." machine

    let rec tick counters buttonIdx acc =
        if buttonIdx = machine.Buttons.Length then
            // We've got to the end with no luck
            // Rotate the array and try again?
            tick (Array.randomShuffle machine.Counters) 0 0
        else if Array.sum counters = 0 then
            acc
        else
            let button = machine.Buttons[buttonIdx]
            let (success, modifiedCounters) = subtractArrays counters button
            
            if success then
                // We go again
                printfn "Pressed %A... state %A" button modifiedCounters
                tick modifiedCounters buttonIdx acc+1
            else
                // Next button
                tick counters (buttonIdx+1) acc

    tick machine.Counters 0 0

    
let run filePath =
    let machines = 
        System.IO.File.ReadAllLines filePath
        |> Array.map parseInputLine

    countButtonPresses machines[0]

printfn "Result: %A" (run "input.txt")