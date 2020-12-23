module Tests

open System
open Xunit
open Day23.Cups

type State = { Cups: int list }

type Game(cups: Map<int, int>, target: int, max: int) =
    let threeAfter value = (cups.[value], cups.[cups.[value]], cups.[cups.[cups.[value]]])
    
    member this.Target = target
    member this.After value = cups.[value]
    
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
        let next = cups
                    |> Map.add source nextTarget
                    |> Map.add third cups.[destination]
                    |> Map.add destination first
        Game(next, nextTarget, max)
        
    member this.PrintFor amt =
        [
            let mutable current = cups.[1]
            for _ in [1 .. amt] do
                yield current
                current <- cups.[current]
        ] 
        
    member this.Print () = this.PrintFor (max - 1) |> List.map string |> List.reduce (+)
    
    
    static member FromList(cups: int array) =
        let len = cups.Length
        let target = cups.[0]

        let cupList =
            cups
            |> Array.mapi (fun i v -> (v, cups.[(i + 1) % len]))
        
        let cupMap = Map.ofArray cupList
        Game(cupMap, target, len)

let playGameRound (game: Game) =
    let destination = game.TargetFor game.Target
    game.Move game.Target destination

let playGameFor rounds (game: Game) =
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
    let game = Game.FromList (example ())
    let ten = playGameFor 10 game
    let hundred = playGameFor 100 game
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

