open Common

let sum = 2020

let compare (array: int[]) =
    Array.sum(array) = sum

let results data =
    data
    |> Seq.filter compare
    |> Seq.head
    |> Array.reduce (fun a b -> a * b)

let lines = Files.read "Input.txt" int

let pairs = seq { 
    for a in lines do
        for b in lines do
            yield [|a; b|]  }

let triplets = seq { 
    for a in lines do
        for b in lines do
            for c in lines do
                yield [|a; b; c|]  }

[<EntryPoint>]
let main _ =
    printfn "Pairs: %d" (results pairs)
    printfn "Triplets: %d" (results triplets)
    0 
