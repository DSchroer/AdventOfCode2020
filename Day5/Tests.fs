module Day5.Tests

open Xunit
open Pass

[<Fact>]
let ``Given a valid pass should know the seat ID``() =
    let pass = "FBFBBFFRLR"
    Assert.Equal({SeatId = 357}, parsePass pass)
    
[<Fact>]
let ``Should find highest pass number``() =
    let highest = passes() |> Seq.max
    Assert.Equal({SeatId = 970}, highest)
    
[<Fact>]
let ``Should find my pass number``() =
    Assert.Equal({SeatId = 587}, myPass())