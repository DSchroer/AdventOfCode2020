module Tests

open System
open Xunit

type Position =
    | Occupied
    | Empty
    | Floor

let charToPosition c =
    match c with
    | '.' -> Floor
    | '#' -> Occupied
    | 'L' -> Empty
    | _ -> failwith "Unsupported char"

let positionToChar c =
    match c with
    | Floor -> '.'
    | Occupied -> '#'
    | Empty -> 'L'

type Seating(layout: string list) =

    member this.Data = layout
    member this.Size = (layout.[0].Length, layout.Length)

    member this.Occupied() =
        this.Data
        |> List.map (fun row ->
            row
            |> Seq.filter ((=) (positionToChar Occupied))
            |> Seq.length)
        |> List.sum

    member this.PositionAt x y =
        let (xMax, yMax) = this.Size
        if (x >= 0 && x < xMax && y >= 0 && y < yMax)
        then layout.[y].[x] |> charToPosition
        else Floor

    override this.GetHashCode() =
        layout
        |> List.map string
        |> List.map (fun a -> a.GetHashCode())
        |> List.reduce ((^^^))

    override this.Equals a =
        a.GetType() = this.GetType()
        && a.GetHashCode() = this.GetHashCode()

type SeatingFactory(layout: string list) =
    let data =
        layout
        |> List.map (fun a -> a.ToCharArray())
        |> List.toArray

    member this.Set x y state = data.[y].[x] <- positionToChar state

    member this.Build() =
        Seating(data |> Array.map String |> Array.toList)

let initialState () =
    Common.Files.readAll "Floorplan.txt" (fun text ->
        Seating
            (text.Split('\n', StringSplitOptions.RemoveEmptyEntries)
             |> Array.toList))

type Rule = { Distance: int; MaxOccupied: int }

let rec trace ((x, y): (int * int)) ((dx, dy): (int * int)) (distance: int) (seating: Seating) =
    let (xMax, yMax) = seating.Size
    if distance = 0 || (x < 0 || x >= xMax || y < 0 || y >= yMax) then
        false
    else
        match seating.PositionAt x y with
        | Occupied -> true
        | Empty -> false
        | Floor -> trace ((x + dx), (y + dy)) (dx, dy) (distance - 1) seating

let applyRules (rule: Rule) x y (builder: SeatingFactory) (seating: Seating) =
    let directions =
        [ (-1, -1)
          (-1, 0)
          (-1, 1)
          (0, -1)
          (0, 1)
          (1, -1)
          (1, 0)
          (1, 1) ]

    let current = seating.PositionAt x y

    let occupied =
        directions
        |> List.filter (fun (dx, dy) -> trace (x + dx, y + dy) (dx, dy) rule.Distance seating)
        |> List.length

    if current = Empty && occupied = 0 then builder.Set x y Occupied
    else if current = Occupied && occupied >= rule.MaxOccupied then builder.Set x y Empty
    else ()

let tick (rule: Rule) (seating: Seating) =
    let builder = SeatingFactory seating.Data
    let (xLength, yLength) = seating.Size
    for x in [ 0 .. xLength ] do
        for y in [ 0 .. yLength ] do
            applyRules rule x y builder seating
    builder.Build()

[<Fact>]
let ``Seatings are the same`` () =
    Assert.Equal(Seating([ "#" ]), Seating([ "#" ]))

[<Fact>]
let ``Can query past bounds`` () =
    let seat = Seating([ "#" ])
    Assert.Equal(Floor, seat.PositionAt 1 1)

[<Fact>]
let ``Can tick simulation`` () =
    let ticker = tick { Distance = 1; MaxOccupied = 4 }
    let seating = initialState ()
    let next = ticker seating
    Assert.NotEqual(seating, next)

[<Fact>]
let ``Simulation settles`` () =
    let ticker = tick { Distance = 1; MaxOccupied = 4 }
    let mutable state = initialState ()
    let mutable next = ticker state
    while next <> state do
        state <- next
        next <- ticker state

    Assert.Equal(2275, state.Occupied())

[<Fact>]
let ``Ray simulation settles`` () =
    let ticker = tick { Distance = 100; MaxOccupied = 5 }
    let mutable state = initialState ()
    let mutable next = ticker state
    while next <> state do
        state <- next
        next <- ticker state

    Assert.Equal(2121, state.Occupied())
