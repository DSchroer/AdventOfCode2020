module Tests

open Day3.Maps
open Xunit

[<Fact>]
let ``Map has tree at 0`` () =
    let map = Map ["#"]
    Assert.True(map.HasTree(0,0))
    
[<Fact>]
let ``Map has no tree at 0`` () =
    let map = Map ["."]
    Assert.False(map.HasTree(0,0))
    
[<Fact>]
let ``Map repeats to the right`` () =
    let map = Map [".#"]
    Assert.False(map.HasTree(2,0))
    Assert.True(map.HasTree(3,0))
    
[<Fact>]
let ``Counts the number of trees``() =
    let slope = {Right = 3; Down = 1}
    let map = input() |> Map
    Assert.Equal(203, (countTrees map slope))
    
[<Fact>]
let ``Product of all slopes``() =
    let map = input() |> Map
    let slopes = [|
        {Right = 1; Down = 1}
        {Right = 3; Down = 1}
        {Right = 5; Down = 1}
        {Right = 7; Down = 1}
        {Right = 1; Down = 2}
    |]
    let distances = slopes
                    |> Array.map (countTrees map)
                    |> Array.map int64
                    |> Array.reduce (fun a b -> a * b)
    Assert.Equal(3316272960L, distances)
