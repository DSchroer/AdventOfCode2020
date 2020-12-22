module Day20.Tile

open System
open Common.Regex

type Side = { Hash: int }

let rev (str: string) = str.ToCharArray() |> Array.rev |> String

type Image(content: string list) =
    let top = content.[0]
    let bottom = rev content.[content.Length - 1]

    let right =
        content
        |> List.map (fun a -> a.[a.Length - 1])
        |> List.toArray
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
        
    member this.Trim() =
        Image(content.[1..content.Length - 2] |> List.map (fun s -> s.[1..s.Length - 2]))
        
    member this.Raw = String.Join("\n", content)

    member this.Count c =
        content
            |> List.map (fun line -> line.ToCharArray() |> Array.filter (fun a -> a = c) |> Array.length)
            |> List.sum
    
    member this.SubsetMatch ((bx,by): int * int) (target: Image) =
        let mutable m = true
        for y in [0 ..target.Content.Length - 1] do
            for x in [0 ..target.Content.[0].Length - 1] do
                let self = content.[by + y].[bx + x]
                let t = target.Content.[y].[x]
                if t = '#' && self <> '#' then m <- false
        m
    
    member this.InstancesOf (target: Image) =
        let mutable instances = 0
        for by in [0 .. content.Length - target.Content.Length] do
            for bx in [0 .. content.[0].Length - target.Content.[0].Length] do
                if this.SubsetMatch (bx, by) target then instances <- instances + 1
        instances
        
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


let parseTile (text: string) =
    let lines =
        text.Split("\n", StringSplitOptions.RemoveEmptyEntries)

    let id =
        match lines.[0] with
        | Regex @"Tile (\d+):" [ id ] -> int id
        | _ -> failwith lines.[0]

    Tile(id, lines |> Seq.skip 1 |> Seq.toList |> Image)
