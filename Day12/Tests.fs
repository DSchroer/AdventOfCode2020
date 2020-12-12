module Tests

open System
open Day12
open Xunit
open Ship
open Common.Files

let input() =
    read "Instructions.txt" (fun a ->
        let value = int a.[1..]
        match a.[0] with
        | 'N' -> {Op = N; Value = value}
        | 'S' -> {Op = S; Value = value}
        | 'E' -> {Op = E; Value = value}
        | 'W' -> {Op = W; Value = value}
        | 'L' -> {Op = L; Value = value}
        | 'R' -> {Op = R; Value = value}
        | 'F' -> {Op = F; Value = value}
        | _ -> failwith "Unknown instruction")

[<Fact>]
let ``Can add vectors`` () =
    let start = {X = 0; Y = 0}
    Assert.Equal({X = 1; Y = 0}, start + {Direction = 0; Magnitude = 1})
    Assert.Equal({X = -1; Y = 0}, start + {Direction = 180; Magnitude = 1})
    Assert.Equal({X = 0; Y = -1}, start + {Direction = -90; Magnitude = 1})
    Assert.Equal({X = 0; Y = 1}, start + {Direction = 90; Magnitude = 1})
    
[<Fact>]
let ``Can calculate distance``() =
    let step (ship: HeadingShip) (action: Action) = ship.Perform action
    let res = input() |> Seq.fold step (HeadingShip(0, { X= 0; Y = 0}))
    Assert.Equal(1010, res.ManhattanDistance (Ship({ X= 0; Y = 0})))
    
[<Fact>]
let ``Can rotate position`` () =
    let start = {X = 5; Y = 0}
    Assert.Equal({X = 5; Y = 0}, rotate start 0)
    Assert.Equal({X = -5; Y = 0}, rotate start 180)
    
    Assert.Equal({X = 0; Y = 5}, rotate start 90)
    Assert.Equal({X = 0; Y = -5}, rotate start -90)

[<Fact>]
let ``Can rotate group`` () =
    let start = {X = 10; Y = 4}
    Assert.Equal({X = 4; Y = -10}, rotate start -90)

[<Fact>]
let ``Can calculate waypoint distance``() =
    let step (ship: WaypointShip) (action: Action) = ship.Perform action
    let res = input() |> Seq.fold step (WaypointShip({ X= 10; Y = 1}, { X= 0; Y = 0}))
    Assert.Equal(52742, res.ManhattanDistance (Ship({ X= 0; Y = 0})))