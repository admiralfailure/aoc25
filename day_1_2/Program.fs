let minValue = 0
let maxValue = 99

let constrain (x) = 
    let change = x
    if change > maxValue then
        minValue
    elif change < minValue then
        maxValue
    else
        change

let constrainedAdd (x, y) = 
    let mutable output = x;
    let mutable currentZeros = 0
    for click in 1..y do
        if output = 0 then
            currentZeros <- currentZeros + 1
            printfn "Current 0s: %d" currentZeros
        output <- constrain (output + 1)

    output, currentZeros


let constrainedSubtract (x, y) = 
    let mutable output = x;
    let mutable currentZeros = 0
    for click in 1..y do
        if output = 0 then
            currentZeros <- currentZeros + 1
            printfn "Current 0s: %d" currentZeros
        output <- constrain (output - 1)
    
    output, currentZeros

let processInput (startPoint, direction, amount) =
    let current = 
        match direction with
            | "L" -> constrainedSubtract (startPoint, amount)
            | "R" -> constrainedAdd (startPoint, amount)
            | _ -> raise (System.ArgumentException("This is knackered"))

    current

let parseInputRow (input: string) =
    let direction = input[0].ToString()
    let amount= input[1..].ToString() |> int

    direction, amount

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let run (path) =
    let mutable totalZeros = 0
    let mutable current = 50
    for line in readLines path do
        let mutable currentZeros = 0
        let direction, amount = parseInputRow (line)
        printfn "Processing input %s %d from %d..." direction amount current
        let (newCurrent, newCurrentZeros) = processInput(current, direction, amount)
        current <- newCurrent
        currentZeros <- newCurrentZeros
        printfn "%d" current

        totalZeros <- totalZeros + currentZeros

    totalZeros
    
printfn "Number of 0s: %d" (run "input.txt")