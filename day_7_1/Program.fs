type TopDownMap(input: char array array) =
    let internalArray = Array2D.init input.Length input[0].Length (fun x y -> (input.[x].[y]))

    let toCoordsAndValue xOffset yOffset input =
        input
        |> Array2D.mapi (fun x y value -> ((x + xOffset, y + yOffset), value))
        |> Seq.cast<((int * int) * char)>

    member this.Item
        with get(x, y) = internalArray[x, y]
        and set(x, y) (value) = internalArray[x, y] <- value

    member this.GetAllNeighbours (x, y) =
        let offsets = [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)]
        this.GetNeighbours (x, y) offsets

    member this.GetAbove (x, y) =
        let offset = [(-1,0)]
        this.GetNeighbours (x, y) offset

    member this.GetBeneath (x, y) =
        let offset = [(1,0)]
        this.GetNeighbours (x, y) offset

    member this.GetSides (x, y) =
        let offsets = [(0,-1); (0,1)]
        this.GetNeighbours (x, y) offsets

    member private this.GetNeighbours (x, y) offsets = 
        offsets
        |> List.choose (fun (dx, dy) ->
            let nx, ny = x + dx, y + dy
            if nx >= 0 && ny >= 0 &&
               nx < Array2D.length1 internalArray &&
               ny < Array2D.length2 internalArray then
                Some ((nx, ny), internalArray[nx, ny])
            else
                None)
        |> List.toArray

    member this.GetAllOfChar search =
        internalArray 
        |> toCoordsAndValue 0 0
        |> Seq.filter (fun (coords, value) -> value = search)
        |> Seq.toArray

    member this.GetNeighboursOfChar search (x, y) =
        this.GetAllNeighbours (x, y)
        |> Seq.filter (fun (coords, value) -> value = search)

    override this.ToString() = 
        for r = 0 to Array2D.length1 internalArray - 1 do
            for c = 0 to Array2D.length2 internalArray - 1 do
                printf "%c" internalArray.[r, c]

            printf "%s" "\r\n"

        ""

let getCellsToUpdate (map: TopDownMap) = 
    // First, do we have any splitters with beams above them?
    let splitters = 
        map.GetAllOfChar '^'
        |> Array.filter(fun (coords, value) -> 
            match map.GetAbove coords with
            | [| (above_coords, above_value) |] -> above_value = '|'
            | _ -> false
        )
        |> Array.map(fun (coords, value) ->
            map.GetSides coords
        )
        |> Array.concat
        |> Array.filter(fun (coords, value) -> 
            value = '.'
        )
        |> Array.map (fun (coords, value) ->
            coords
        )

    // Next, do we have any beams with space beneath them?
    let beams = 
        map.GetAllOfChar '|'
        |> Array.map(fun (coords, value) -> 
            map.GetBeneath coords
        )
        |> Array.filter(fun beneath -> 
            match beneath with
            | [| (beneath_coords, beneath_value) |] -> beneath_value = '.'
            | _ -> false
        )
        |> Array.map (fun beneath ->
            let (coords, value) = beneath |> Array.head
            coords
        )

    // Finally, do we have an S with no beam beneath it?
    let spawners = 
        map.GetAllOfChar 'S'
        |> Array.map(fun (coords, value) -> 
            map.GetBeneath coords
        )
        |> Array.filter(fun beneath -> 
            match beneath with
            | [| (beneath_coords, beneath_value) |] -> beneath_value <> '|'
            | _ -> false
        )
        |> Array.map (fun beneath ->
            let (coords, value) = beneath |> Array.head
            coords
        )

    [|splitters; beams; spawners|]

let tick (map: TopDownMap) = 
    match getCellsToUpdate map with
    | [|splitters; beams; spawners|] -> 
        splitters
        |> Array.iter(fun (x, y) -> map[x, y] <- '|')

        beams
        |> Array.iter(fun (x, y) -> map[x, y] <- '|')

        spawners
        |> Array.iter(fun (x, y) -> map[x, y] <- '|')

        //printfn "%O" map

        (splitters.Length / 2, splitters.Length + beams.Length + spawners.Length)
    | _ -> (0, 0)

let process (map: TopDownMap) =
    let rec simulate map totalSplits =
        let (splits, total) = tick map

        if total = 0 then
            totalSplits
        else
            simulate map (splits + totalSplits)

    simulate map 0

let buildMapFromFile filePath =
    System.IO.File.ReadAllLines filePath
        |> Array.map Seq.toArray
        |> TopDownMap

let run filePath =
    let map = buildMapFromFile filePath
    process map 

printfn "%A" (run "input.txt")