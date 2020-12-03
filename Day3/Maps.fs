module Day3.Maps

open Common

type Map(data: string seq) =
    let values = data |> Seq.toArray

    let charAt (x, y) =
        let row = values.[y]
        row.[x % row.Length]

    member this.HasTree(x, y) = charAt (x, y) = '#'
    member this.End(x, y) = y >= values.Length

type Slope = { Right: int; Down: int }

let rec treesFrom x y (slope: Slope) (map: Map) =
    if map.End(x, y) then 0
    else
        let current = if map.HasTree(x, y) then 1 else 0
        current + treesFrom (x + slope.Right) (y + slope.Down) slope map

let countTrees map slope =
    treesFrom 0 0 slope map

let input () = Files.read "Map.txt" string
