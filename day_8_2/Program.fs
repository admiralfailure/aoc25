type Point = { X: int64; Y: int64; Z: int64 }

let cartesianDistance point1 point2 =
    let x = pown (point1.X - point2.X) 2
    let y = pown (point1.Y - point2.Y) 2
    let z = pown (point1.Z - point2.Z) 2
    let total = (x + y + z) |> float

    sqrt total

let parseRow (row: string) =
    match row.Split ',' with
    | [| x; y; z |] -> { X = x |> int64; Y = y |> int64; Z = z |> int64 }
    | _ -> failwith "Unable to parse input row"

let buildDistancesMap (points: Point array) =
    let map: float[,] = Array2D.init points.Length points.Length (fun x y -> 0.0)

    let indexedPoints =
        points 
        |> Array.indexed

    for (idx1, point1) in indexedPoints do
        for (idx2, point2) in indexedPoints do
            // Are we matching ourselves?
            if point1 = point2 then
                map[idx1, idx2] <- 0.0
                map[idx2, idx1] <- 0.0
            // Calculate and populate
            else if map[idx1, idx2] = 0.0 then
                let distance = cartesianDistance point1 point2

                map[idx1, idx2] <- distance
                map[idx2, idx1] <- 0.0  // This is a bit weird, must be a nicer way
            // Something went wrong
            //else if map[idx1, idx2] <> map[idx2, idx1] then
            //    failwithf "Distances out of sync: %A <> %A [%A, %A]" (map[idx1, idx2]) (map[idx2, idx1]) idx1 idx2
    map

let sortDistancesMap map =
    map
    |> Array2D.mapi(fun x y value ->
        x, y, value
    )
    |> Seq.cast<(int * int * float)>
    |> Seq.toArray
    |> Array.filter(fun (x, y, value) -> value > 0.0)
    |> Array.sortBy(fun (x, y, value) -> value)

let existsInCircuit circuits value =
    let matches =
        circuits
        |> Array.indexed
        |> Array.choose(fun (idx, circuit) ->
            if Set.contains value circuit then
                Some (idx, circuit)
            else
                None
        )

    match matches with
    | [| (idx, circuit) |] -> Some (idx, circuit)
    | [||] -> None
    | _ -> failwith "Invalid circuit match lookup!"

let processConnection circuits x y =
    let xCircuit = existsInCircuit circuits x
    let yCircuit = existsInCircuit circuits y

    // CASES
    // - Neither in a circuit: create new circuit and add both
    if xCircuit = None && yCircuit = None then
        Array.append circuits [| Set[x; y] |] |> Array.filter(fun x-> x.Count > 0)
    // - X in a circuit, Y in no circuit: add Y to X's circuit
    else if xCircuit <> None && yCircuit = None then
        let (idx, circuit) = xCircuit.Value
        circuits[idx] <- circuit.Add y
        circuits |> Array.filter(fun x-> x.Count > 0)
    // - Y in a circuit, X in no circuit: add X to Y's circuit
    else if xCircuit = None && yCircuit <> None then
        let (idx, circuit) = yCircuit.Value
        circuits[idx] <- circuit.Add x
        circuits |> Array.filter(fun x-> x.Count > 0)
    // - X in a circuit, Y in a different circuit: merge X and Y's circuits
    else if xCircuit <> None && yCircuit <> None && xCircuit <> yCircuit then
        let (xIdx, xCircuit) = xCircuit.Value
        let (yIdx, yCircuit) = yCircuit.Value

        circuits[xIdx] <- Set.union xCircuit yCircuit
        circuits[yIdx] <- Set.empty
        circuits |> Array.filter(fun x-> x.Count > 0)
    // - X in a circuit, Y in the same circuit: nothing to do
    else
        circuits 

let getLastConnectionToCompleteCircuit (points: Point array) (sortedDistances: (int * int * float) array) =
    let rec connect circuits idx =
        let connection = sortedDistances[idx]
        let (x, y, value) = connection

        let processedCircuits = processConnection circuits x y

        if processedCircuits.Length = 1 then
            (x, y)
        else
            connect processedCircuits (idx + 1)
       
    let initialCircuits = Array.init points.Length (fun x -> Set[x])
    connect initialCircuits 0

let run filePath =
    let points = 
        System.IO.File.ReadAllLines filePath
        |> Array.map parseRow

    let sortedDistances =
        buildDistancesMap points
        |> sortDistancesMap

    let (toIdx, fromIdx) = getLastConnectionToCompleteCircuit points sortedDistances

    let toPoint = points[toIdx]
    let fromPoint = points[fromIdx]

    toPoint.X * fromPoint.X

printfn "Result: %A" (run "input.txt")