open System
open Day23
open Tests
open Cups

module Program =
    [<EntryPoint>]
    let main _ =
        Console.WriteLine("Building state...")
        let cups = expand 1_000_000 (input ())
        let game = Game.FromList cups

        Console.WriteLine("Playing rounds...")
        let endStates = playGameFor 10_000_000 game
        let output = endStates.PrintFor 2
        let first = int64 output.[0]
        let second = int64 output.[1]
        Console.WriteLine(first * second)
        0
