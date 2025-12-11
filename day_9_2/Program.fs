type Coord = { X: int; Y: int }

type Direction =
    | Vertical
    | Horizontal

type EdgeCell = { X: int; Y: int; Direction: Direction}
type Rectangle = { (*EdgeCoords: Set<Coord>;*) Corner1: Coord; Corner2: Coord; Area: int64 }

type Map(input: string array) = 
    let redTiles = 
        input
        |> Array.map(fun x ->
            match x.Split ',' with
            | [| xStr; yStr |] -> { X = xStr |> int; Y = yStr |> int }
            | _ -> failwith "Unable to parse Coord from input"
        )

    let getCoordsBetween (coord1: Coord) (coord2: Coord) = seq {
        // Either X matches...
        if coord1.X = coord2.X then
            for y in [(min coord1.Y coord2.Y)..(max coord1.Y coord2.Y)] do
                yield { X = coord1.X; Y = y; Direction = Direction.Vertical }
        // or Y matches
        else if coord1.Y = coord2.Y then
            for x in [(min coord1.X coord2.X)..(max coord1.X coord2.X)] do
                yield { X = x; Y = coord1.Y; Direction = Direction.Horizontal }
        else 
            failwith "Unable to build edges"
    }

    let edges =
        redTiles
        |> Array.indexed
        |> Array.fold(fun acc (idx, coord) ->
            if idx = 0 then
                let previous = Array.last redTiles
                Set.union acc (Set.ofSeq(getCoordsBetween previous coord))
            else 
                let previous = redTiles[idx-1]
                Set.union acc (Set.ofSeq(getCoordsBetween previous coord))
        ) Set.empty<EdgeCell>
        //|> Set.union (Set.ofArray redTiles)
        |> Set.toArray      // Will change

    //let mutable insideShape = Set.union (Set.ofArray(redTiles)) (Set.ofArray edges)
    //let mutable outsideShape = Set.empty<Coord>

    let calculateArea (coord1: Coord) (coord2: Coord) =
        let side1 = (max coord1.X coord2.X) - (min coord1.X coord2.X) + 1
        let side2 = (max coord1.Y coord2.Y) - (min coord1.Y coord2.Y) + 1

        (side1 |> int64) * (side2 |> int64)

    //member this.getEdgeCoords coord1 coord2 = seq {
    //    yield! getCoordsBetween coord1 { X = coord1.X; Y = coord2.Y }
    //    yield! getCoordsBetween { X = coord1.X; Y = coord2.Y } coord2
    //    yield! getCoordsBetween coord2 { X = coord2.X; Y = coord1.Y }
    //    yield! getCoordsBetween { X = coord2.X; Y = coord1.Y } coord1
        
    //    // Set.ofList(List.concat [edge1; edge2; edge3; edge4])
    //}

    member this.getCorners rect =
        let corner1 = rect.Corner1
        let corner2 = { X = rect.Corner1.X; Y = rect.Corner2.Y }
        let corner3 = rect.Corner2
        let corner4 = { X = rect.Corner2.X; Y = rect.Corner1.Y }

        (corner1, corner2, corner3, corner4)

    member this.crossedEdge (coord1: Coord) (coord2: Coord) =
        // Either X matches...
        if coord1.X = coord2.X then
            Array.exists(fun edge -> edge.Direction = Horizontal && edge.X = coord1.X && edge.Y > (min coord1.Y coord2.Y) && edge.Y < (max coord1.Y coord2.Y)) edges
        // or Y matches
        else if coord1.Y = coord2.Y then
            Array.exists(fun edge -> edge.Direction = Vertical && edge.Y = coord1.Y && edge.X > (min coord1.X coord2.X) && edge.X < (max coord1.X coord2.X)) edges
        else 
            failwith "Unable to build edges"


    member this.CalculateAreas = seq {
        for idx1 = 0 to redTiles.Length - 1 do
            for idx2 = idx1 + 1 to redTiles.Length - 1 do
                let coord1 = redTiles[idx1]
                let coord2 = redTiles[idx2]

                yield { 
                    Area = calculateArea coord1 coord2;
                    //EdgeCoords = Set.ofSeq(this.getEdgeCoords coord1 coord2)
                    Corner1 = coord1;
                    Corner2 = coord2;
                }
    }

    member this.Width = 
        (redTiles
            |> Array.map(fun coord -> coord.X)
            |> Array.max) + 1 

    member this.Height =
        (redTiles
            |> Array.map(fun coord -> coord.Y)
            |> Array.max) + 1

    //member private this.touchesBounds beam =
    //    beam
    //    |> Seq.exists(fun x -> edges.Contains x)
        
    //member this.IsWithinShape coord =
    //    if insideShape.Contains coord then
    //        true
    //    else if outsideShape.Contains coord then
    //        false
    //    else
    //        // If you can travel in any one direction and hit the borders without 
    //        // first hitting a red or green tile, you're outside the shape

    //        let outside = not(Array.exists (fun edge -> edge.X=coord.X && edge.Y <= coord.Y) edges) || not(Array.exists (fun edge -> edge.X=coord.X && edge.Y >= coord.Y) edges) || not(Array.exists (fun edge -> edge.X <= coord.X && edge.Y = coord.Y) edges) || not(Array.exists (fun edge -> edge.X >= coord.X && edge.Y = coord.Y) edges)

            
    //        //let upBeam = getCoordsBetween coord { X = coord.X; Y = 0 }
    //        //let downBeam = getCoordsBetween coord { X = coord.X; Y = this.Height - 1 }
    //        //let leftBeam = getCoordsBetween coord { X = 0; Y = coord.Y }
    //        //let rightBeam = getCoordsBetween coord { X = this.Width - 1; Y = coord.Y }

    //        //let outside = (not(this.touchesBounds leftBeam) || not(this.touchesBounds rightBeam) || not(this.touchesBounds upBeam) || not(this.touchesBounds downBeam))

    //        if outside then
    //            //printfn "Caching outside value [%d]..." outsideShape.Count
    //            //outsideShape <- outsideShape.Add coord
    //            false
    //        else
    //            //printfn "Caching inside value [%d]..." insideShape.Count
    //            //insideShape <- insideShape.Add coord
    //            true

    //member this.Print rect =
    //    // How big is our map?
    //    let maxX = 
    //        redTiles
    //        |> Array.map(fun coord -> coord.X)
    //        |> Array.max 

    //    let maxY = 
    //        redTiles
    //        |> Array.map(fun coord -> coord.Y)
    //        |> Array.max 

    //    printfn "Printing %dx%d map..." maxX maxY

    //    for y = 0 to maxY + 1 do
    //        for x = 0 to maxX + 1 do
    //            let edgeCoords = Set.ofSeq(this.getEdgeCoords rect.Corner1 rect.Corner2)
    //            if edgeCoords.Contains({ X = x; Y = y }) then
    //                printf "%c" 'O'
    //            else if Array.contains { X = x; Y = y } redTiles then
    //                printf "%c" '#'
    //            else if Array.contains { X = x; Y = y } edges then
    //                printf "%c" 'X'
    //            else
    //                printf "%c" '.'

    //        printfn "" 

    //    printfn "Done"
        
let run filePath =
    printfn "Starting work..."

    let map = 
        System.IO.File.ReadAllLines filePath
        |> Map

    printfn "Generated map..."

    let allRects = map.CalculateAreas

    printfn "Calculated areas..."

    let rectArray = 
        allRects
        |> Seq.toArray

    let rectCount = rectArray.LongLength
    printfn "Converted to array of size %d..." rectCount

    let sortedRects =
        rectArray
        |> Array.sortBy(fun rect -> -rect.Area)

    printfn "Sorted rects..."
        
    let largestValidRect = snd (
        sortedRects
        |> Array.indexed
        |> Array.find(fun (idx, rect) -> 
            //printfn "Processing rect %d/%d [%d]" (idx+1) (rectCount) rect.Area

            let (corner1, corner2, corner3, corner4) = map.getCorners rect

            if map.crossedEdge corner1 corner2 then
                //printfn "Crossed edge between %A and %A" corner1 corner2
                false
            else if map.crossedEdge corner2 corner3 then
                //printfn "Crossed edge between %A and %A" corner2 corner3
                false
            else if map.crossedEdge corner3 corner4 then
                //printfn "Crossed edge between %A and %A" corner3 corner4
                false
            else if map.crossedEdge corner4 corner1 then
                //printfn "Crossed edge between %A and %A" corner4 corner1
                false
            else 
                true
            

            //edges
            //|> Seq.forall(fun coord -> 
            //    printfn "Checking coord %A" coord
            //    map.IsWithinShape coord
            //)
        )
    )

    printfn "Found largest valid rect..."

    largestValidRect.Area


printfn "Result: %A" (run "input.txt")