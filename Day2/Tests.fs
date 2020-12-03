module Day2.Tests

open Xunit
open Day2.Program

[<Fact>]
let ``Has policy1 passwords``() =
    Assert.Equal(536, (validPasswords policy1))

[<Fact>]
let ``Has policy2 passwords``() =
    Assert.Equal(558, (validPasswords policy2))