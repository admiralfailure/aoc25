let firstOccurrenceOfMaxValue input startIndex endIndex =
    Seq.indexed input
    |> Seq.filter (fun x -> (fst x) >= startIndex && (fst x) <= endIndex)
    |> Seq.maxBy snd 

let getBankJoltage digits (bank: string) =
    [1..digits]
    |> Seq.rev
    |> Seq.scan (fun digitOfOutput (index) -> 
            firstOccurrenceOfMaxValue bank (fst digitOfOutput + 1) (bank.Length - index)
        ) (-1, ' ')
    |> Seq.skip 1
    |> Seq.map snd
    |> System.String.Concat
    |> int64

let run path =
    System.IO.File.ReadLines path
    |> Seq.map (getBankJoltage 12)
    |> Seq.sum


printfn "Total max joltage: %A" (run "input.txt")