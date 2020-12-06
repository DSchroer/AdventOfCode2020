module Day6.Tests

open Xunit
open Declarations

[<Fact>]
let ``Should count all sets that anyone answered`` () =
    let sum = input() anyoneAnswered
              |> Array.sumBy (fun a -> a.Count)
    Assert.Equal(7120, sum)
    
[<Fact>]
let ``Can union single``() =
    let text = "abc"
    let result = everyoneAnswered text |> Set.count
    Assert.Equal(3, result)
    
[<Fact>]
let ``Can union single char``() =
    let text = "a"
    let result = everyoneAnswered text |> Set.count
    Assert.Equal(1, result)
    
[<Fact>]
let ``Can handle duplicates``() =
    let text = "aaabbb"
    let result = everyoneAnswered text |> Set.count
    Assert.Equal(2, result)
    
[<Fact>]
let ``Can union multiple``() =
    let text = "a\nb\nc"
    let result = everyoneAnswered text |> Set.count
    Assert.Equal(0, result)
    
[<Fact>]
let ``Can union multiple repeating``() =
    let text = "a\nab\na"
    let result = everyoneAnswered text |> Set.count
    Assert.Equal(1, result)
    
[<Fact>]
let ``Can union grouping``() =
    let text = "ab\nac"
    let result = everyoneAnswered text |> Set.count
    Assert.Equal(1, result)
    
[<Fact>]
let ``Should count all sets that everyone answered`` () =
    let sets = input() everyoneAnswered
    let sum = sets |> Array.sumBy (fun a -> a.Count)
    Assert.Equal(3570, sum)