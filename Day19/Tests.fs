module Tests

open System
open Day19
open Xunit
open Rule
open Common.Files

let input replace =
    let rules = (read "Rules.txt" parseRule |> Seq.toList) @ replace
    let lines = read "Lines.txt" string
    (lines, RuleSet(Map(rules)))
    
let exInput replace =
    let rules = (read "ExRules.txt" parseRule |> Seq.toList) @ replace
    let lines = read "ExLines.txt" string
    (lines, RuleSet(Map(rules)))
    
let exampleRules () =
    let data = """
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"
"""
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Array.map parseRule |> Map |> RuleSet
    
let exampleLines () =
    let data = """
ababbb
bababa
abbbab
aaabbb
aaaabbb
"""
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    
[<Fact>]
let ``Can match example`` () =
    let set = exampleRules()
    let lines = exampleLines()
    let matches = lines |> Seq.filter (set.Matches 0) |> Seq.length
    Assert.Equal(2, matches)
    
//[<Fact>]
//let ``Can match example by printing`` () =
//    let set = exampleRules()
//    let lines = exampleLines()
//    let options = set.Print 0
//    let matches = lines |> Seq.filter options.Matches |> Seq.length
//    Assert.Equal(2, matches)
    
let replacements () =
    [
        (8, (Join ((Refs [42]), (Rec8 42))))
        (11, (Join ((Refs [42; 31]), (Rec11 (42, 31)))))
    ]
    
[<Fact>]
let ``Can match part one`` () =
    let (lines, set) = input([])
    let matches = lines |> Seq.filter (set.Matches 0) |> Seq.length
    Assert.Equal(109, matches)
    
//[<Fact>]
//let ``Can match part one by printing`` () =
//    let (lines, set) = input([])
//    let options = set.Print 0
//    let matches = lines |> Seq.filter options.Matches |> Seq.length
//    Assert.Equal(109, matches)
    
//[<Fact>]
//let ``Can match part two`` () =
//    let (lines, set) = input(replacements ())
//    let matches = lines |> Seq.filter (set.Matches 0) |> Seq.length
//    Assert.Equal(109, matches)
    
[<Fact>]
let ``Options work``() =
    Assert.True(Options(["a"]).Matches "a")
    Assert.True(Options(["b"; "aa"]).Matches "aa")
    
//[<Fact>]
let ``Can test options``() =
    let (lines, set) = exInput(replacements ())
    let matches = lines |> Seq.filter (set.Matches 0) |> Seq.length
    Assert.Equal(12, matches)
    
//[<Fact>]
let ``Can test options1``() =
    let (lines, set) = exInput(replacements ())
    let mutable amt = 0
    for line in lines do
        let options = set.Print line 0
        if options.Matches line then amt <- amt + 1
    Assert.Equal(12, amt)