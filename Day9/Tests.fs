module Tests

open System
open Common.Files
open Xunit

let isValid (preamble: int64 list) target =
    let options =
        [ for i in preamble do
            for j in preamble do
                if not (i = j) then yield (i, j) ]

    let valid =
        options
        |> List.tryFind (fun (i, j) -> (i + j) = target)

    valid.IsSome

let isValidIndex (code: int64 list) index =
    if index <= 25 then true
    else
        let preamble = code.[index - 26 .. index - 1]
        let target = code.[index]
        isValid preamble target

let findSequence (code: int64 list) target =
    let index = code |> List.findIndex ((=) target)
    let res = [for i in [0 .. index - 1] do
                for j in [i + 1 ..  index - 1] do
                    let sum = code.[i..j] |> List.sum
                    if sum = target then code.[i..j]]
    res

[<Fact>]
let ``Can locate valid result`` () =
    let data = read "Code.txt" int64 |> Seq.toList

    let invalid =
        data
        |> List.mapi (fun index _ -> isValidIndex data index)
        |> List.findIndex (fun a -> not a)

    Assert.Equal(1212510616L, data.[invalid])

[<Fact>]
let ``Can find sequence`` () =
    let data = read "Code.txt" int64 |> Seq.toList

    let run = Assert.Single(findSequence data 1212510616L) |> List.sort
    
    Assert.Equal(1212510616L, run |> List.sum)
    Assert.Equal(171265123L, run.[0] + run.[run.Length - 1])