open System
open Common

let sum = 2020
let lines = Files.read "Input.txt" int

let compare (a,b) =
    a + b = sum

let options = seq { 
    for a in lines do
        for b in lines do
            yield (a, b)  }

let results =
    options
    |> Seq.filter compare

let result =
    results
    |> Seq.head
    |> fun (a, b) -> a * b

[<EntryPoint>]
let main _ =
    Console.WriteLine(result)
    0 
