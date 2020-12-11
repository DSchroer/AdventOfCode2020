module Day10.Tests_ts

open Common
open Xunit
open Common.Files

let findMin (jolts: int list) =
    let min = jolts |> List.min
    let index = jolts |> List.findIndex ((=) min)
    let rest = (jolts.[ .. index - 1] @ jolts.[index + 1 .. ])
    (min, rest)
    
let diffJolt target (jolts: int list) =
    let (current, rest) = findMin jolts
    let rec oneJolt last (jolts: int list) = 
        let (current, rest) = findMin jolts
        let value = if current - last = target then 1 else 0
        if rest.Length > 0 then value + (oneJolt current rest)
        else value
    oneJolt current rest
        
let fullSet (jolts: int list) =
    let max = jolts |> List.max
    [0] @ jolts @ [max + 3]
        
let testData() = [
    28
    33
    18
    42
    31
    14
    46
    20
    48
    47
    24
    23
    49
    45
    19
    38
    39
    11
    1
    32
    25
    35
    8
    17
    7
    9
    4
    2
    34
    10
    3
]

let input() =
    Files.read "Jolts.txt" int |> Seq.toList

[<Fact>]
let ``Can count number of mins``() =
    let ones = diffJolt 1 (fullSet (testData()))
    Assert.Equal(22, ones)

[<Fact>]
let ``Can count number of maxs``() =
    let threes = diffJolt 3 (fullSet (testData()))
    Assert.Equal(10, threes)
    
[<Fact>]
let ``Can find full diff``() =
    let ones = diffJolt 1 (fullSet (input()))
    let threes = diffJolt 3 (fullSet (input()))
    Assert.Equal(2775, ones * threes)


let countArrangements (jolts: int list) =
    let counts = Array.zeroCreate<int64> jolts.Length
    counts.[jolts.Length - 1] <- 1L
    
    for pos in {0 .. jolts.Length - 2} |> Seq.rev  do
        let mutable ptr = pos + 1
        while ptr < jolts.Length && jolts.[ptr] - jolts.[pos] <= 3 do
            counts.[pos] <- counts.[pos] + counts.[ptr]
            ptr <- ptr + 1
        
    counts.[0]
         
[<Fact>]
let ``Can count number of arrangements``() =
    let arrangements = countArrangements (fullSet (testData() |> List.sort))
    Assert.Equal(19208L, arrangements)
    
[<Fact>]
let ``Can count full number of arrangements``() =
    let arrangements = countArrangements (fullSet (input() |> List.sort))
    Assert.Equal(518_344_341_716_992L, arrangements)
