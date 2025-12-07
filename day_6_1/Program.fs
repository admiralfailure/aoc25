let parseInput filePath =
    let input = System.IO.File.ReadAllLines filePath
    match input with
    | [| numbers_0; numbers_1; numbers_2; numbers_3; operators |] -> 
        let numbersArray_0 = numbers_0.Split ' ' |> Array.filter (fun x -> x <> "") |> Array.map int64
        let numbersArray_1 = numbers_1.Split ' ' |> Array.filter (fun x -> x <> "") |> Array.map int64
        let numbersArray_2 = numbers_2.Split ' ' |> Array.filter (fun x -> x <> "") |> Array.map int64
        let numbersArray_3 = numbers_3.Split ' ' |> Array.filter (fun x -> x <> "") |> Array.map int64
        let operatorsArray = operators.Split ' ' |> Array.filter (fun x -> x <> "")

        (numbersArray_0, numbersArray_1, numbersArray_2, numbersArray_3, operatorsArray)
    | _ -> failwith "Unable to parse input"

let performOperation number_0 number_1 number_2 number_3 operator =
    match operator with
    | "+" -> number_0 + number_1 + number_2 + number_3
    | "*" -> number_0 * number_1 * number_2 * number_3
    |_ -> failwith "Unknown operator"

let run filePath =
    let numbersArray_0, numbersArray_1, numbersArray_2, numbersArray_3, operatorsArray = parseInput filePath

    operatorsArray
    |> Array.mapi (fun idx operator ->
        performOperation numbersArray_0[idx] numbersArray_1[idx] numbersArray_2[idx] numbersArray_3[idx] operator
    )
    |> Array.sum


printfn "Result: %A" (run "input.txt")
    