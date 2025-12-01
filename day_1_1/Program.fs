let minValue = 0
let maxValue = 99

let constrain (x) = 
    let change = x % 100
    if change > maxValue then
        change - maxValue - 1
    elif change < minValue then
        change + maxValue + 1
    else
        change

let constrainedAdd (x, y) = 
    x + y
    |> constrain

let constrainedSubtract (x, y) =
    x - y
    |> constrain

let processInput (startPoint, direction, amount) =
    match direction with
        | "L" -> constrainedSubtract (startPoint, amount)
        | "R" -> constrainedAdd (startPoint, amount)
        | _ -> raise (System.ArgumentException("This is knackered"))

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
    let mutable zeros = 0
    let mutable current = 50
    for line in readLines path do
        let direction, amount = parseInputRow (line)
        printfn "Processing input %s %d from %d..." direction amount current
        current <- processInput(current, direction, amount)
        printfn "%d" current

        if current = 0 then
            zeros <- zeros + 1

    zeros
    
printfn "Number of 0s: %d" (run "input.txt")