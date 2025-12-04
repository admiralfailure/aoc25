type TopDownMap<'char>(input: char array array) =
    let internalArray = Array2D.init input.Length input[0].Length (fun x y -> (input.[x].[y]))

    let toCoordsAndValue xOffset yOffset input =
        input
        |> Array2D.mapi (fun x y value -> ((x + xOffset, y + yOffset), value))
        |> Seq.cast<((int * int) * char)>

    member this.Item
        with get(x, y) = internalArray[x, y]
        and set(x, y) (value) = internalArray[x, y] <- value

    member this.GetNeighbours (x, y) =
        let rowStart = if y - 1 >= 0 then y - 1 else 0
        let rowEnd = if y + 1 <= Array2D.length2 internalArray then y + 1 else y
        let colStart = if x - 1 >= 0 then x - 1 else 0
        let colEnd = if x + 1 <= Array2D.length1 internalArray then x + 1 else x

        internalArray[colStart..colEnd, rowStart..rowEnd]
        |> toCoordsAndValue colStart rowStart
        |> Seq.filter(fun (coords, value) -> coords <> (x, y))
        |> Seq.toArray

    member this.GetAllOfChar search =
        internalArray 
        |> toCoordsAndValue 0 0
        |> Seq.filter (fun (coords, value) -> value = search)

    member this.GetNeighboursOfChar search (x, y) =
        this.GetNeighbours (x, y)
        |> Seq.filter (fun (coords, value) -> value = search)



let buildMapFromFile filePath =
    System.IO.File.ReadAllLines filePath
        |> Seq.map Seq.toArray
        |> Seq.toArray
        |> TopDownMap

let getAccessibleRolls (map: TopDownMap<char>) search neighbourLimit =
    map.GetAllOfChar search 
    |> Seq.filter (fun (coords, _) -> 
        Seq.length (map.GetNeighboursOfChar search coords) < neighbourLimit
    )
    |> Seq.toList

let removeAccessibleRolls (map: TopDownMap<char>) search neighbourLimit =
    let accessibleRolls = getAccessibleRolls map search neighbourLimit
    let count = Seq.length accessibleRolls

    accessibleRolls
    |> Seq.iter (fun ((x, y), _) -> map[x, y] <- '.')

    count

let rec removeAllAccessibleRolls (map: TopDownMap<char>) search neighbourLimit acc = 
    let removed = removeAccessibleRolls map search neighbourLimit
    if removed = 0 then
        acc
    else 
        removeAllAccessibleRolls map search neighbourLimit (acc + removed)

let run filePath search neighbourLimit = 
    let map = buildMapFromFile filePath
    removeAllAccessibleRolls map search neighbourLimit 0

printfn "Result: %A" (run "input.txt" '@' 4)