let getSplitPoints operators = 
    let (results, idx) =
        operators
        |> Seq.toArray
        |> Array.fold(fun (results, idx) current ->
            match current with
            | ' ' -> (results, idx + 1)
            | _ -> (idx :: results, idx + 1)
        ) ([], 0)

    List.rev (results)
    |> List.skip 1

let splitAtPoints (splitPoints: int list) (input: string) =
    let maxIdx = splitPoints.Length - 1

    let parts = 
        splitPoints
        |> List.indexed
        |> List.fold(fun result (idx, point) ->
            match idx with
            | 0 -> input[0..point-2] :: result
            | _ -> input[splitPoints[idx-1]..point-2] :: result
        ) [""]
    
    List.rev (input[splitPoints[maxIdx]..] :: parts)
    |> List.skip 1

let parseInput filePath =
    let input = System.IO.File.ReadAllLines filePath

    // Last line is the operators
    let operators = input[input.Length - 1]

    // Get the points to split the strings
    let splitPoints = getSplitPoints operators

    // Split the rows
    let numbers =
        input
        |> Array.take (input.Length - 1)
        |> Array.map (splitAtPoints splitPoints) 
        |> Array.toList

    // Clean up the operators and prepend them
    (operators.Split ' ' |> Array.filter (fun x -> x <> "") |> Array.toList) :: numbers

let readNumberFromColumn (input: string list) =
    input[0]
    |> Seq.toArray
    |> Array.indexed
    |> Array.fold(fun result (idx, value) ->
        let column = 
            input
            |> List.map(fun item -> item[idx])
            |> System.String.Concat

        column :: result
    ) []
    |> List.map int64
    
let performOperation (numbers: int64 list) operator =
    match operator with
    | "+" -> List.sum numbers
    | "*" -> List.fold (fun acc cur -> acc * cur) (1) numbers
    |_ -> failwith "Unknown operator"

let processInput (input: string list list) =
    let operators = 
        input
        |> List.take 1
        |> List.concat

    let numbers =
        input
        |> List.skip 1

    let rotated = 
        [0..(operators.Length - 1)]
        |> List.map(fun idx ->
            numbers
            |> List.map (fun items -> items[idx])
        )

    rotated
    |> List.map readNumberFromColumn
    |> List.mapi(fun idx numbers ->
        performOperation numbers operators[idx]
    )
    |> List.sum

    
let run filePath =
    parseInput filePath
    |> processInput

    
printfn "Result: %A" (run "input.txt")
    