module Day1.Tests

open Day1.Program
open Xunit

[<Fact>]
let ``Has pairs result`` () =
    Assert.Equal(802011, lines () |> pairs |> results)

[<Fact>]
let ``Has triplets result`` () =
    Assert.Equal(248607374, lines () |> triplets |> results)
