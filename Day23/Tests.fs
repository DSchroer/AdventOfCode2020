module Tests

open System
open Xunit
open Day23.Cups

type IGame =
    abstract Target: int
    abstract TargetFor: int -> int
    abstract Move: int -> int -> IGame
    abstract PrintFor: int -> int list
    abstract Print: Unit -> string

type Game(cups: int array, target: int, max: int) =
    let threeAfter value = (cups.[value], cups.[cups.[value]], cups.[cups.[cups.[value]]])
    
    member this.After value = cups.[value]
    
    interface IGame with
        member this.Target = target
        member this.TargetFor value =
            let (first, second, third) = threeAfter value
            let exclude = [value; first; second; third]
            let mutable target = value - 1
            
            if target <= 0 then target <- max
            while (exclude |> List.contains target) do
                target <- target - 1
                if target <= 0 then target <- max
                
            target
    
        member this.Move source destination =
            let (first, _, third) = threeAfter source
            let nextTarget = cups.[third]
            
            cups.[source] <- nextTarget
            cups.[third] <- cups.[destination]
            cups.[destination] <- first
   
            Game(cups, nextTarget, max) :> IGame
        
        member this.PrintFor amt =
            [
                let mutable current = cups.[1]
                for _ in [1 .. amt] do
                    yield current
                    current <- cups.[current]
            ] 
        
        member this.Print () = (this :> IGame).PrintFor (max - 1) |> List.map string |> List.reduce (+)
    
    
    static member FromList(cups: int array) =
        let len = cups.Length
        let target = cups.[0]

        let indexed = Array.create (len + 1) 0
        for (i, v) in cups |> Array.indexed do
            indexed.[v] <- cups.[(i + 1) % len]
        
        Game(indexed, target, len)

let playGameRound (game: IGame) =
    let destination = game.TargetFor game.Target
    game.Move game.Target destination

let playGameFor rounds (game: IGame) =
    let mutable current = game
    for _ in 1 .. rounds do
        current <- playGameRound current
    current

[<Fact>]
let ``Can play map version``() =
    let game = Game.FromList (example())
    let first = playGameRound game
    Assert.Equal("54673289", first.Print ())
    let second = playGameRound first
    Assert.Equal("32546789", second.Print ())
    
[<Fact>]
let ``Can complete example`` () =    
    let ten = playGameFor 10 (Game.FromList (example ()))
    let hundred = playGameFor 100 (Game.FromList (example ()))
    Assert.Equal("92658374", ten.Print ())
    Assert.Equal("67384529", hundred.Print ())

[<Fact>]
let ``Can complete part 1`` () =
    let game = Game.FromList (input ())
    let hundred = playGameFor 100 game
    Assert.Equal("53248976", hundred.Print())

let expand target (cups: int array) =
    let max = cups |> Array.max
    Array.concat [cups; [|max + 1 ..target|]]

[<Fact>]
let ``Can complete example part 2`` () =
    let cups = expand 1_000_000 (example ())
    let game = Game.FromList cups

    let endStates = playGameFor 10_000_000 game
    let output = endStates.PrintFor 2
    let first = int64 output.[0]
    let second = int64 output.[1]
    
    Assert.Equal(149245887792L, first * second)
    
[<Fact>]
let ``Can complete part 2`` () =
    let cups = expand 1_000_000 (input ())
    let game = Game.FromList cups

    let endStates = playGameFor 10_000_000 game
    let output = endStates.PrintFor 2
    let first = int64 output.[0]
    let second = int64 output.[1]
    
    Assert.Equal(418819514477L, first * second)