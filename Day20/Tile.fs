module Day20.Tile

open System
open Common.Regex

type Side = { Hash: int }

let rev (str: string) = str.ToCharArray() |> Array.rev |> String

type Image(content: string list) =
    let top = content.[0]
    let bottom = content.[content.Length - 1]

    let right =
        content
        |> List.map (fun a -> a.[a.Length - 1])
        |> List.toArray
        |> Array.rev
        |> String

    let left =
        content
        |> List.map (fun a -> a.[0])
        |> List.toArray
        |> Array.rev
        |> String
        
    

    let side data = { Hash = data.GetHashCode() }

    member private this.Content = content
    member this.Top = top |> side
    member this.Bottom = bottom |> side
    member this.Left = left |> side
    member this.Right = right |> side

    member this.Sides =
        [ this.Top
          this.Left
          this.Right
          this.Bottom ]

    member this.Flip() =
        let flipped = content |> List.map rev
        Image(flipped)

    member this.Rotate() =
        let sizeX = content.[0].Length
        let sizeY = content.Length
        let nI = Array2D.create sizeX sizeY (char 0)

        for x in [ 0 .. sizeX - 1 ] do
            for y in [ 0 .. sizeY - 1 ] do
                nI.[x, y] <- content.[sizeY - y - 1].[x]
                
        let next = [0 ..sizeX - 1] |> List.map (fun x -> nI.[x, 0 .. sizeY - 1] |> String) 
           
        Image(next)
        
    member this.Print() =
        String.Join("\n", content)
        
    static member MergeRight (a: Image) (b: Image) =
        Image(a.Content |> List.mapi (fun i s -> s + b.Content.[i]))
    static member MergeDown (a: Image) (b: Image) =
        Image(a.Content @ b.Content)
        
    
        

type Tile(id: int, image: Image) =
    member this.Id = id
    member this.Image = image
    member this.Flipped = image.Flip()
    member this.AllSides = Set(this.Image.Sides @ this.Flipped.Sides)

    static member CanConnect (a: Tile) (b: Tile) =
        let intersect =
            Set.intersect (a.AllSides) (b.AllSides)

        intersect.Count > 0

type Grid(connections: (Tile * Tile list) list) =

    let findRightTile (image: Image) (possible: Tile list) =
        let side = image.Right

        let tile =
            possible
            |> List.find (fun i -> i.AllSides.Contains side)

        let mutable next = if Set(tile.Image.Sides).Contains side then tile.Image else tile.Flipped
        
        while next.Left <> side do
            next <- next.Rotate()

        (tile.Id, next)
        
    let findBottomTile (image: Image) (possible: Tile list) =
        let side = image.Bottom

        let tile =
            possible
            |> List.find (fun i -> Set(i.AllSides).Contains side)

        let mutable next = if Set(tile.Image.Sides).Contains side then tile.Image else tile.Flipped
        
        while next.Top <> side do
            next <- next.Rotate()

        (tile.Id, next)

    let findConnections (id: int) =
        let (_, c) = connections |> List.find (fun (t, _) -> t.Id = id)
        c
    
    let size = int (sqrt (float connections.Length))

    let grid = Array2D.create size size (Image([ " " ]))
    let idGrid =  Array2D.create size size (0)

    do
        let (tile, cn) =
            connections
            |> List.filter (fun (_, c) -> c.Length = 2)
            |> List.find (fun (t, c) ->
                let points =
                    c
                    |> List.map (fun c -> c.AllSides)
                    |> List.reduce (Set.union)
                    |> Set

                points.Contains(t.Flipped.Bottom)
                && points.Contains(t.Flipped.Right))

        let top = tile.Flipped
        let (_, right) = findRightTile top cn
        let (_, bottom) = findBottomTile top cn
        
        let image = Image.MergeDown (Image.MergeRight top right) bottom
        let view = image.Print()
        
        for y in [0 .. size - 1] do
            for x in [0 .. size - 1] do
                if x = 0 && y = 0
                then
                    grid.[x,y] <- tile.Flipped
                    idGrid.[x,y] <- tile.Id
                else 
                    let (id, image) =
                        if x = 0
                        then findBottomTile (grid.[x, y - 1]) (findConnections idGrid.[x, y - 1])
                        else findRightTile (grid.[x - 1, y]) (findConnections idGrid.[x - 1, y])
                    grid.[x, y] <- image
                    idGrid.[x, y] <- id
                
    member this.Data = grid
    member this.Print () =
        Image.MergeRight grid.[0,0] grid.[1, 0]
        

let parseTile (text: string) =
    let lines =
        text.Split("\n", StringSplitOptions.RemoveEmptyEntries)

    let id =
        match lines.[0] with
        | Regex @"Tile (\d+):" [ id ] -> int id
        | _ -> failwith lines.[0]

    Tile(id, lines |> Seq.skip 1 |> Seq.toList |> Image)
