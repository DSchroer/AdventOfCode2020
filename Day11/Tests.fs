module Tests

open System
open Xunit

type Seating =
    | Occupied
    | Empty
    | Floor

let seatingToChar seating =
    match seating with
    | Occupied -> '#'
    | Empty -> 'L'
    | Floor -> '.'

let charToSeating char =
    match char with
    | '#' -> Occupied
    | 'L' -> Empty
    | '.' -> Floor
    | _ -> failwith "Unexpected seating type"

type Arrangement(data: char array array) =

    override this.GetHashCode() = 0

    override this.Equals a =
        a.GetType() = this.GetType()
        && a.GetHashCode() = this.GetHashCode()

    member this.Data = data
    member this.SetSeating x y seating = data.[y].[x] <- seatingToChar seating

    member this.GetSeating x y =
        let (xMax, yMax) = this.Size()
        if x >= 0 && x < xMax && y >= 0 && y < yMax then charToSeating data.[y].[x] else Floor

    member this.Size() = (data.[0].Length, data.Length)

[<Fact>]
let ``My test`` () = Assert.True(true)
