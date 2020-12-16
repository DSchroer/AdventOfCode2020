module Tests

open System
open Xunit

type Memory() =
    let mutable index = 0
    let mutable values = Map.empty<int, int>
    member this.Index = index
    member this.Say number =
        index <- index + 1
        let lastIndex = if values.ContainsKey number then Some values.[number] else None
        values <- values.Add(number, index)
        lastIndex

let runGame (start: int seq) (len: int) =
    seq {
        let memory = Memory ()
        let mutable li = None;
        for n in start do
            li <- memory.Say n
            yield n
        let mutable value = 0
        for _ in [memory.Index .. len - 1] do
            match li with
            | Some last -> value <- (memory.Index - last)
            | None -> value <- 0
            li <- memory.Say value
            yield value
    }

[<Fact>]
let ``Can follow example`` () =
    let gameSeq = runGame (seq {0;3;6}) 10
    let expected = seq {0; 3; 6; 0; 3; 3; 1; 0; 4; 0} |> Seq.toList
    Assert.Equal(expected, gameSeq);

let startingNumbers () =
    seq {
        8
        11
        0
        19
        1
        2
    }
    
[<Fact>]
let ``Can predict 2020`` () =
    let gameSeq = runGame (startingNumbers ()) 2020
    Assert.Equal(447, gameSeq |> Seq.last)
    
//[<Fact>]
// Takes a while
let ``Can predict 30_000_000`` () =
    let gameSeq = runGame (startingNumbers ()) 30_000_000
    Assert.Equal(11721679, gameSeq |> Seq.last);