type Coord = { X: int64; Y: int64 }

let buildCoords (input: string array) =
    input
    |> Array.map(fun x ->
        match x.Split ',' with
        | [| xStr; yStr |] -> { X = xStr |> int64; Y = yStr |> int64 }
        | _ -> failwith "Unable to parse Coord from input"
    )

let calculateArea coord1 coord2 =
    let side1 = (max coord1.X coord2.X) - (min coord1.X coord2.X) + 1L
    let side2 = (max coord1.Y coord2.Y) - (min coord1.Y coord2.Y) + 1L

    side1 * side2

let calculateAreas (coords: Coord array) = seq {
    for idx1 = 0 to coords.Length - 1 do
        for idx2 = idx1 + 1 to coords.Length - 1 do
            let coord1 = coords[idx1]
            let coord2 = coords[idx2]

            yield calculateArea coord1 coord2
}

let run filePath =
    System.IO.File.ReadAllLines filePath
    |> buildCoords
    |> calculateAreas
    |> Seq.max

printfn "Result: %A" (run "input.txt")