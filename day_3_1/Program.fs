let firstOccurrenceOfMaxValue input =
    Seq.indexed input
    |> Seq.maxBy snd 
         
let maxJoltage (input: string) =
    firstOccurrenceOfMaxValue input[0..-2]
    |> fun firstDigit -> seq {
        [| snd firstDigit; snd (firstOccurrenceOfMaxValue input[fst firstDigit + 1..]) |]
    }
    |> Array.concat 
    |> System.String

let run path =
    System.IO.File.ReadLines path
    |> Seq.map maxJoltage
    |> Seq.map int
    |> Seq.sum

printfn "Total max joltage: %A" (run "input.txt")