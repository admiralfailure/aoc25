type Machine = { IndicatorLights: int16; Buttons: int16 list }

let parseInputLine (line: string) =
    let parts = line.Split ' '

    //printfn "Split into parts: %A" parts

    let indicatorPart = (parts[0])[1..parts[0].Length - 2]
    //printfn "Indicator part: %A" indicatorPart

    let target = System.Convert.ToInt16(indicatorPart.Replace('.', '0').Replace('#', '1').Trim(), 2)

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
                [0..indicatorPart.Length-1]
                |> Seq.map(fun idx ->
                    //printfn "Checking idx %A" idx
                    if Array.contains idx ones then
                        '1'
                    else
                        '0'
                )
                |> Seq.toArray
                |> System.String

            //printfn "Binary: %A" binary

            System.Convert.ToInt16(binary, 2)
        )
        |> Array.toList

    { IndicatorLights = target; Buttons = buttons }

// Rewrite this to be clearer
let rec combinations elements inputList = 
    match elements, inputList with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) inputList) @ combinations k xs

let xorMany (combo: int16 list) =
    combo
    |> List.fold(fun acc cur ->
        acc ^^^ cur
    ) 0s
    
let run filePath =
    let machines = 
        System.IO.File.ReadAllLines filePath
        |> Array.map parseInputLine

    let matchingComboLengths =
        machines
        |> Array.map(fun machine ->
            let combos = seq {
                for elems=1 to machine.Buttons.Length do
                    yield! combinations elems machine.Buttons
            }

            let matchingCombo = 
                combos
                |> Seq.find(fun combo -> 
                    (xorMany combo) = machine.IndicatorLights
                )

            matchingCombo.Length
        )

    Array.sum matchingComboLengths

printfn "Result: %A" (run "input.txt")