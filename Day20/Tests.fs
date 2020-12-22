//module Tests
//
//open System
//open Day20
//open Xunit
//open Common.Files
//open Tile
//
//let input() =
//    readAll "Tiles.txt" (fun all ->
//        let blocks = all.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
//        blocks |> Array.map parseTile) |> Array.toList
//
//type Tiles = Tile list
//    
//let connections (tiles: Tiles) =
//    [
//        for tile in tiles do
//            let connections = tiles
//                              |> List.filter (fun t -> t.Id <> tile.Id)
//                              |> List.filter (fun t -> Tile.CanConnect tile t)
//            yield (tile, connections)
//            ]
//
//[<Fact>]
//let ``Can rotate images``() =
//    let img = Image(["..#"; "..."; "..."; "..."])
//    Assert.Equal(img.Top, img.Rotate().Right)
//    Assert.Equal(img.Top, img.Rotate().Rotate().Bottom)
//    Assert.Equal(img.Top, img.Rotate().Rotate().Rotate().Left)
//    Assert.Equal(img.Top, img.Rotate().Rotate().Rotate().Rotate().Top)
//
//[<Fact>]
//let ``Has one way to flip``() =
//    let img = Image(["1234"; "5678"; "9*@#"; "%^&("])
//    let flip = img.Flip ()
//    Assert.Equal(0, (Set.intersect (Set img.Sides) (Set flip.Sides)).Count)
//
//[<Fact>]
//let ``Can find corners`` () =
//    let sides =  input()
//                 |> connections
//                 |> List.filter (fun (_, c) -> c.Length = 2)
//                 |> List.map (fun (t, _) -> t)
//                 |> Seq.toList
//                 
//    Assert.Equal(4, sides.Length)
//    Assert.Equal(12519494280967L, sides |> List.map (fun t -> int64 t.Id) |> List.reduce (*))
//    
//[<Fact>]
//let ``Can find all connections`` () =
//    let sides =  input() |> connections |> Seq.toList
//    let g = Grid(sides).Print().Print()
//    ()