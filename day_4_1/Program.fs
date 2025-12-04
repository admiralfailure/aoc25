type TopDownMap<'char>(input: char array array) =
    let internalArray = Array2D.init input.Length input[0].Length (fun x y -> (input.[x].[y]))

    member this.Item
        with get(x, y) = internalArray[x, y]
        
    member this.GetWithNeighbours (x, y) =
        let rowStart = if y - 1 >= 0 then y - 1 else 0
        let rowEnd = if y + 1 <= Array2D.length2 internalArray then y + 1 else y
        let colStart = if x - 1 >= 0 then x - 1 else 0
        let colEnd = if x + 1 <= Array2D.length1 internalArray then x + 1 else x

        internalArray[colStart..colEnd, rowStart..rowEnd]
        |> Seq.cast<char>
        |> Seq.toArray

    member this.GetAllOfChar search =
        internalArray 
        |> Array2D.mapi (fun x y value -> ((x, y), value))
        |> Seq.cast<((int * int) * char)>
        |> Seq.filter (fun (coords, value) -> value = search)
        |> Seq.map (fun (coords, value) -> coords)

    member this.GetCountOfNeighboursOfChar search (x, y) =
        this.GetWithNeighbours (x, y)
        |> Seq.filter (fun neighbour -> neighbour = search)
        |> Seq.skip 1
        |> Seq.length

let buildMapFromFile filePath =
    System.IO.File.ReadAllLines filePath
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> TopDownMap

let run filePath = 
    let map = buildMapFromFile filePath

    map.GetAllOfChar '@' 
    |> Seq.map (map.GetCountOfNeighboursOfChar '@')
    |> Seq.filter (fun x -> x < 4)
    |> Seq.length  

printfn "Result: %A" (run "input.txt")