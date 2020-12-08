module Day8.Tests

open Day8.Runtime
open Xunit

[<Fact>]
let ``Can create a program``() =
    let program = bootloader () |> Seq.toList |> Program
    Assert.NotNull(program) 
    
[<Fact>]
let ``Can detect the loop``() =
    let program = bootloader () |> Seq.toList |> Program
    execute program |> ignore
    Assert.Equal(120, program.ProgramCounter)
    Assert.Equal(1394, program.Accumulator)

[<Fact>]
let ``Can patch the loop``() =
    let instructions = bootloader () |> Seq.toList
    let patches = patch instructions
    Assert.Equal(1626, Assert.Single(patches).Accumulator)
