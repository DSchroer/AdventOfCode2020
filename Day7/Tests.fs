module Day7.Tests

open Xunit

[<Fact>]
let ``Can parse rules``() =
    let text = "light red bags contain 1 bright white bag, 2 muted yellow bags."
    let parsed = Parser.parseRule text
    Assert.Equal("light red", parsed.Name)
    Assert.Equal(1, parsed.Contain.["bright white"])
    Assert.Equal(2, parsed.Contain.["muted yellow"])

[<Fact>]
let ``Can parse empty rules``() =
    let text = "light red bags contain no other bags."
    let parsed = Parser.parseRule text
    Assert.Equal("light red", parsed.Name)
    Assert.True(parsed.Contain.IsEmpty)

[<Fact>]
let ``Can parse rule file``() =
    Assert.NotEmpty(Parser.parseAllRules())

[<Fact>]
let ``Can list golden containers``() =
    let rules = Parser.parseAllRules()
    let canHold = Rules.canHold (rules |> Array.ofSeq) "shiny gold"
    Assert.Equal(326, canHold.Length)

[<Fact>]
let ``Gold contains how many bags``() =
    let rules = Parser.parseAllRules()
    let canHold = Rules.holdsHowMany (rules |> Array.ofSeq) "shiny gold"
    Assert.Equal(5635, canHold)
