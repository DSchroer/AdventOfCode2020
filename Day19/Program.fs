module Program

open System
open System.Threading
open System.Threading.Tasks
open Tests

let [<EntryPoint>] main _ =
    let (lines, set) = input(replacements ())
    let mutable amt = ref 0
    Parallel.ForEach(lines, (fun line ->
        let options = set.Print line 0
        if options.Matches line then Interlocked.Increment(amt) |> ignore
        Console.WriteLine(line)
    )) |> ignore
    Console.WriteLine(amt.Value)
    0
