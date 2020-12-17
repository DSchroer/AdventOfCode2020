module Tests

open System
open Xunit

type IVec =
    inherit IComparable
    abstract member Around: Unit -> IVec seq
    
type State = Alive | Dead
    
type World(points: IVec seq) =
    let alive = Set<IVec> points
    member this.Item
        with get (index: IVec) =
            if alive.Contains index then Alive
            else Dead
    member this.Interest () =
        alive
        |> Seq.map (fun v -> v.Around ())
        |> Seq.concat
    member this.Count = alive.Count
    
let step (world: World) =
    let nextState (world: World) (loc: IVec) =
        let self = world.[loc]
        let occupied = loc.Around ()
                        |> Seq.filter ((<>)loc)
                        |> Seq.filter (fun v -> world.[v] = Alive)
                        |> Seq.length
        if self = Alive && (occupied = 2 || occupied = 3) then Alive
        else if self = Dead && occupied = 3 then Alive
        else Dead
    
    let next = world.Interest() |> Seq.filter (fun l -> (nextState world l) = Alive)
    World next
    
let simulateFor steps world =
    let mutable current = world
    for _ in [0..(steps - 1)] do current <- step current
    current
    
let parse (ctor: (int * int) -> IVec) (text: string) =
    let lines = text.Split('\n', StringSplitOptions.RemoveEmptyEntries)
    seq {
        for x in [0 .. lines.[0].Length - 1] do
            for y in [0 .. lines.Length - 1] do
                if lines.[y].[x] = '#' then
                    yield ctor (x,y)
    }
    
type Vec3 = {X: int; Y: int; Z: int} with
    interface IVec with 
        member this.Around() =
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
    static member From ((x,y): (int * int)): IVec =
        upcast {X=x; Y=y; Z=0} 
        
type Vec4 = {X: int; Y: int; Z: int; W: int}
    with
    interface IVec with
        member this.Around () =
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
    static member From ((x,y): (int * int)): IVec =
        upcast {X=x; Y=y; Z=0; W=0}
    
[<Fact>]
let ``Can simulate`` () =
    let start = parse Vec3.From """
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
    let start = parse Vec3.From (input ())
    let final = simulateFor 6 (World start)
    Assert.Equal(336, final.Count)
    

[<Fact>]
let ``Can simulate 4 dim`` () =
    let start = parse Vec4.From """
.#.
..#
###
"""
    let final = simulateFor 6 (World start)
    Assert.Equal(848, final.Count)

[<Fact>]
let ``Can simulate main input 4 dim`` () =
    let start = parse Vec4.From (input ())
    let final = simulateFor 6 (World start)
    Assert.Equal(2620, final.Count)
    