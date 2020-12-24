module Tests

open System
open Day24
open Xunit
open Common.Regex
open Common.Files
open HexGrid

let parseInstructions (line: string) =
    seq {
        let mutable remaining = line
        while remaining.Length <> 0 do
            match remaining with
            | Regex "^se" [] ->
                remaining <- remaining.[2..]
                yield SE
            | Regex "^sw" [] ->
                remaining <- remaining.[2..]
                yield SW
            | Regex "^ne" [] ->
                remaining <- remaining.[2..]
                yield NE
            | Regex "^nw" [] ->
                remaining <- remaining.[2..]
                yield NW
            | Regex "^e" [] ->
                remaining <- remaining.[1..]
                yield E
            | Regex "^w" [] ->
                remaining <- remaining.[1..]
                yield W
            | _ -> failwith "Unexpected direction"
    }

let example () = read "ExampleTiles.txt" parseInstructions
let input () = read "Tiles.txt" parseInstructions

let setupGrid (rules: Direction seq seq) =
    let foldPosition (pos: Position) (dir: Direction) = pos.Move dir
    let tilePos = rules |> Seq.map (fun dirs -> dirs |> Seq.fold foldPosition {X = 0; Y = 0})
    let grid = HexGrid()
    for pos in tilePos do
        grid.Flip pos
    grid

let nextColor (grid: HexGrid) (pos: Position) =
    let color = grid.[pos]
    let blackAround = pos.Around()
                      |> Seq.filter ((<>)pos)
                      |> Seq.filter (fun p -> grid.[p] = Black)
                      |> Seq.length
    if color = Black && (blackAround = 0 || blackAround > 2) then (pos, White)
    else if color = White && blackAround = 2 then (pos, Black)
    else (pos, color)

let step (grid: HexGrid) =
    let interest = grid.Interest()
    let next = interest |> Seq.map (nextColor grid) |> Seq.toList
    for (pos, color) in next do
        grid.Set pos color
    grid
    
let stepFor days grid =
    let mutable state = grid
    for _ in 1 .. days do
        state <- step state
    state

[<Fact>]
let ``Can complete example`` () =
    let rules = example()
    let grid = setupGrid rules
    
    Assert.Equal(10, grid.CountBlack())
    
[<Fact>]
let ``Can complete part 1`` () =
    let rules = input()
    let grid = setupGrid rules
    
    Assert.Equal(420, grid.CountBlack())
    
[<Fact>]
let ``Can complete example part 2`` () =
    let rules = example()
    let grid = setupGrid rules
    
    let final = stepFor 100 grid
    
    Assert.Equal(2208, final.CountBlack())
    
[<Fact>]
let ``Can complete part 2`` () =
    let rules = input()
    let grid = setupGrid rules
    
    let final = stepFor 100 grid
    
    Assert.Equal(4206, final.CountBlack())