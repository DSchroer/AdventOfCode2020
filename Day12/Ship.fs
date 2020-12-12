module Day12.Ship

open System

type ActionOp = N | S | E | W | L | R | F

type Action = {
    Op: ActionOp
    Value: int
}

type Position = {X: int; Y: int}

type Vector = { Direction: int; Magnitude: int }

let degToRad x =
    x * (Math.PI / 180.)

type Position with
    static member (+) (position: Position, vector: Vector) =
        let (x, y) = (position.X, position.Y)
        let (dir, mag) = (vector.Direction |> float |> degToRad, vector.Magnitude)
        { X = x + ((cos dir |> int) * mag); Y = y + ((sin dir |> int) * mag) }
    static member (*) (position: Position, scalar: int) =
        { X = position.X * scalar; Y = position.Y * scalar }
    static member (+) (position: Position, pos2: Position) =
        { X = position.X + pos2.X; Y = position.Y + pos2.Y}

let rotate (position: Position) (angle: int) =
    let (x, y) = (position.X, position.Y)
    let dir = angle |> float |> degToRad
    let c = (cos dir |> int)
    let s = (sin dir |> int)
    {
        X = (x * c) - (y * s)
        Y = (x * s) + (y * c)
    }

type Ship(position: Position) =
    member private this.Position = position
    member this.ManhattanDistance (ship: Ship) =
        let orig = ship.Position
        let self = this.Position
        (abs (self.X - orig.X)) + (abs (self.Y - orig.Y))

type HeadingShip(heading: int, position: Position) =
    inherit Ship(position)
    member this.Perform (action: Action) =
        match action.Op with
        | N -> HeadingShip(heading, position + { Direction = 90; Magnitude = action.Value })
        | S -> HeadingShip(heading, position + { Direction = -90; Magnitude = action.Value })
        | E -> HeadingShip(heading, position + { Direction = 0; Magnitude = action.Value })
        | W -> HeadingShip(heading, position + { Direction = 180; Magnitude = action.Value })
        | L -> HeadingShip(heading + action.Value, position)
        | R -> HeadingShip(heading - action.Value, position)
        | F -> HeadingShip(heading, position + { Direction = heading; Magnitude = action.Value })

 
type WaypointShip(waypoint: Position, position: Position) =
    inherit Ship(position)
    member this.Perform (action: Action) =
        match action.Op with
        | N -> WaypointShip(waypoint + { Direction = 90; Magnitude = action.Value }, position)
        | S -> WaypointShip(waypoint + { Direction = -90; Magnitude = action.Value }, position)
        | E -> WaypointShip(waypoint + { Direction = 0; Magnitude = action.Value }, position)
        | W -> WaypointShip(waypoint + { Direction = 180; Magnitude = action.Value }, position)
        | L -> WaypointShip(rotate waypoint action.Value, position)
        | R -> WaypointShip(rotate waypoint (-action.Value), position)
        | F -> WaypointShip(waypoint, position + (waypoint * action.Value))
