module Tests

open System
open Xunit

type Vec = {X: int; Y: int; Z: int}
    with
    static member Around (this: Vec) =
        seq {
            for x in [-1..1] do
                for y in [-1..1] do
                    for z in [-1..1] do
                        yield {
                            X = this.X + x
                            Y = this.Y + y
                            Z = this.Z + z
                        }
        }
        
type VecW = {X: int; Y: int; Z: int; W: int}
    with
    static member Around (this: VecW) =
        seq {
            for x in [-1..1] do
                for y in [-1..1] do
                    for z in [-1..1] do
                        for w in [-1..1] do
                            yield {
                                X = this.X + x
                                Y = this.Y + y
                                Z = this.Z + z
                                W = this.W + w
                            }
        }
    
type State = Alive | Dead
    
type World(points: Vec seq) =
    let alive =  Set<Vec> points
    member this.Item
        with get (index: Vec) =
            if alive.Contains index then Alive
            else Dead
    member this.Interest () =
        alive
        |> Seq.map Vec.Around
        |> Seq.concat
    member this.Count = alive.Count
    
type WorldW(points: VecW seq) =
    let alive =  Set<VecW> points
    member this.Item
        with get (index: VecW) =
            if alive.Contains index then Alive
            else Dead
    member this.Interest () =
        alive
        |> Seq.map VecW.Around
        |> Seq.concat
    member this.Count = alive.Count

let nextState (world: World) (loc: Vec) =
    let self = world.[loc]
    let occupied = Vec.Around loc
                    |> Seq.filter ((<>)loc)
                    |> Seq.filter (fun v -> world.[v] = Alive)
                    |> Seq.length
    if self = Alive && (occupied = 2 || occupied = 3) then Alive
    else if self = Dead && occupied = 3 then Alive
    else Dead
    
let nextStateW (world: WorldW) (loc: VecW) =
    let self = world.[loc]
    let occupied = VecW.Around loc
                    |> Seq.filter ((<>)loc)
                    |> Seq.filter (fun v -> world.[v] = Alive)
                    |> Seq.length
    if self = Alive && (occupied = 2 || occupied = 3) then Alive
    else if self = Dead && occupied = 3 then Alive
    else Dead
    
let step (world: World) =
    let next = world.Interest()
               |> Seq.filter (fun l -> (nextState world l) = Alive)
    World next
    
let stepW (world: WorldW) =
    let next = world.Interest()
               |> Seq.filter (fun l -> (nextStateW world l) = Alive)
    WorldW next

let simulateFor steps world =
    let mutable current = world
    for _ in [0..(steps - 1)] do current <- step current
    current
    
let simulateForW steps world =
    let mutable current = world
    for _ in [0..(steps - 1)] do current <- stepW current
    current

let parse (text: string) =
    let lines = text.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    seq {
        for x in [0 .. lines.[0].Length - 1] do
            for y in [0 .. lines.Length - 1] do
                if lines.[y].[x] = '#' then
                    yield {X=x; Y=y; Z=0}
    }
    
let parseW (text: string) =
    let lines = text.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    seq {
        for x in [0 .. lines.[0].Length - 1] do
            for y in [0 .. lines.Length - 1] do
                if lines.[y].[x] = '#' then
                    yield {X=x; Y=y; Z=0; W=0}
    }

[<Fact>]
let ``Can simulate`` () =
    let start = parse """
.#.
..#
###
"""
    let final = simulateFor 6 (World start)
    Assert.Equal(112, final.Count)
    
let input() =
    """
#####..#
#..###.#
###.....
.#.#.#..
##.#..#.
######..
.##..###
###.####
"""
    
[<Fact>]
let ``Can simulate main input`` () =
    let start = parse (input ())
    let final = simulateFor 6 (World start)
    Assert.Equal(336, final.Count)
    

[<Fact>]
let ``Can simulateW`` () =
    let start = parseW """
.#.
..#
###
"""
    let final = simulateForW 6 (WorldW start)
    Assert.Equal(848, final.Count)

[<Fact>]
let ``Can simulate main inputW`` () =
    let start = parseW (input ())
    let final = simulateForW 6 (WorldW start)
    Assert.Equal(2620, final.Count)
    