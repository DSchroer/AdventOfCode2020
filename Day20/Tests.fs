module Tests

open System
open Day20
open Xunit
open Common.Files
open Tile

let input() =
    readAll "Tiles.txt" (fun all ->
        let blocks = all.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
        blocks |> Array.map parseTile) |> Array.toList

type Tiles = Tile list
    
let connections (tiles: Tiles) =
    [
        for tile in tiles do
            let connections = tiles
                              |> List.filter (fun t -> t.Id <> tile.Id)
                              |> List.filter (fun t -> Tile.CanConnect tile t)
            yield (tile, connections)
            ]
    

[<Fact>]
let ``Can find corners`` () =
    let corners =  input()
                 |> connections
                 |> List.filter (fun (_, c) -> c.Length = 2)
                 |> List.map (fun (t, _) -> t)
                 |> Seq.toList
                 
    Assert.Equal(4, corners.Length)
    Assert.Equal(12519494280967L, corners |> List.map (fun t -> int64 t.Id) |> List.reduce (*))
    
let findRight (image: Image) (tiles: Tiles) =
    let right = image.Right
    let tile = tiles |> List.find (fun t -> t.AllSides.Contains right)
    let selected = if Set(tile.Image.Sides).Contains right then tile.Image else tile.Flipped
    
    let nextImage = 
                    if right = selected.Top then selected.Flip().Rotate().Rotate().Rotate()
                    else if right = selected.Left then selected.Flip().Rotate().Rotate()
                    else if right = selected.Bottom then selected.Flip().Rotate()
                    else if right = selected.Right then selected.Flip()
                    else failwith "Unsupported alignment"
    (tile.Id, nextImage)
    
let findBottom (image: Image) (tiles: Tiles) =
    let bottom = image.Bottom
    let tile = tiles |> List.find (fun t -> t.AllSides.Contains bottom)
    let selected = if Set(tile.Image.Sides).Contains bottom then tile.Image else tile.Flipped
    
    let nextImage = 
                    if bottom = selected.Top then selected.Flip()
                    else if bottom = selected.Left then selected.Flip().Rotate().Rotate().Rotate()
                    else if bottom = selected.Bottom then selected.Flip().Rotate().Rotate()
                    else if bottom = selected.Right then selected.Flip().Rotate()
                    else failwith "Unsupported alignment"
    (tile.Id, nextImage)
    
let buildImage () =
    let connections = input() |> connections
    let (tr, c) = connections
                   |> List.filter (fun (_, c) -> c.Length = 2)
                   |> List.find (fun (i, c) ->
                       let sides = Set.union c.[0].AllSides c.[1].AllSides
                       sides.Contains(i.Flipped.Bottom) && sides.Contains(i.Flipped.Right))
    
    let dataFor id =
        connections |> List.find (fun (t, _) -> t.Id = id)
    
    let idGrid = Array2D.create 12 12 0
    let grid = Array2D.create 12 12 (Image(["X"]))
    
    idGrid.[0,0] <- tr.Id
    grid.[0,0] <- tr.Flipped
    
    for y in [0 .. 11] do
        for x in [0 .. 11] do
            if x = 0 && y = 0 then ()
            else if x = 0 then
                let (_, con) = dataFor idGrid.[x, y - 1]
                let img = grid.[x, y - 1]
                let (nextId, nextImg) = findBottom img con
                idGrid.[x, y] <- nextId
                grid.[x, y] <- nextImg
            else
                let (_, con) = dataFor idGrid.[x - 1, y]
                let img = grid.[x - 1, y]
                let (nextId, nextImg) = findRight img con
                idGrid.[x, y] <- nextId
                grid.[x, y] <- nextImg 
    grid
    
let combineImage (grid: Image [,]) =
    [0 .. 11]
                |> List.map (fun y -> grid.[0..11,y] |> Array.reduce Image.MergeRight)
                |> List.reduce Image.MergeDown
    
let monster() =
    Image([
        "                  # "
        "#    ##    ##    ###"
        " #  #  #  #  #  #   "
    ])
    
    
[<Fact>]
let ``Can find all connections`` () =
    let image = buildImage()
    let trimmed = image |> Array2D.map (fun i -> i.Trim())
    
    let search = (combineImage trimmed).Flip().Rotate()
    let searchHashes = search.Count '#'
    
    let monster = monster()
    let monsterHashes = monster.Count '#'
    
    let monsters = search.InstancesOf monster
    Assert.Equal(2442, searchHashes - (monsters * monsterHashes))