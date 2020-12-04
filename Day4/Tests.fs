module Day4.Tests

open Xunit
open Day4.Passport
open Day4.Parser
open Xunit

[<Fact>]
let ``Empty passports are invalid``() =
    let passport = Passport([||])
    Assert.False(isValid passport)
    
[<Fact>]
let ``Passports with all fields are valid``() =
    let passport = Passport([|
        {Name = "byr"; Value = ""}
        {Name = "iyr"; Value = ""}
        {Name = "eyr"; Value = ""}
        {Name = "hgt"; Value = ""}
        {Name = "hcl"; Value = ""}
        {Name = "ecl"; Value = ""}
        {Name = "pid"; Value = ""}
        {Name = "cid"; Value = ""}
    |])
    Assert.True(isValid passport)
    
[<Fact>]
let ``Passports missing cid are valid``() =
    let passport = Passport([|
        {Name = "byr"; Value = ""}
        {Name = "iyr"; Value = ""}
        {Name = "eyr"; Value = ""}
        {Name = "hgt"; Value = ""}
        {Name = "hcl"; Value = ""}
        {Name = "ecl"; Value = ""}
        {Name = "pid"; Value = ""}
    |])
    Assert.True(isValid passport)
  
[<Fact>]
let ``Given input should parse passports``() =
    Assert.NotEmpty(input())
    
[<Fact>]
let ``Should know valid passport length``() =
    let length = input() |> Array.filter isValid |> Array.length
    Assert.Equal(190, length)

[<Fact>]
let ``Should identify strict valid passport``() =
    let text = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"
    Assert.True(isStrictValid (parsePassport text))

[<Fact>]
let ``Should know strict valid passport length``() =
    let length = input() |> Array.filter isStrictValid |> Array.length
    Assert.Equal(121, length)