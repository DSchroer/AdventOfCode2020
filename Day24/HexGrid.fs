module Day24.HexGrid

type Direction =
    | E
    | SE
    | SW
    | W
    | NW
    | NE

type Position =
    { X: int
      Y: int }
    member this.Move direction =
        match direction with
        | E -> { X = this.X + 1; Y = this.Y }
        | W -> { X = this.X - 1; Y = this.Y }
        | SW -> { X = this.X - 1; Y = this.Y - 1 }
        | SE -> { X = this.X; Y = this.Y - 1 }
        | NW -> { X = this.X; Y = this.Y + 1 }
        | NE -> { X = this.X + 1; Y = this.Y + 1 }

    member this.Around() =
        seq {
            yield this
            yield this.Move E
            yield this.Move W
            yield this.Move SW
            yield this.Move SE
            yield this.Move NW
            yield this.Move NE
        }

type Tile =
    | White
    | Black
    member this.Flip() = if this = White then Black else White

type HexGrid() =
    let mutable tiles = Set.empty<Position>

    member this.Item pos =
        if tiles.Contains pos then Black else White

    member this.Flip pos =
        tiles <- if tiles.Contains pos then tiles.Remove pos else tiles.Add pos

    member this.Set pos tile =
        if tile = Black && not (tiles.Contains pos) then tiles <- tiles.Add pos
        if tile = White && tiles.Contains pos then tiles <- tiles.Remove pos
        
    member this.CountBlack() = tiles.Count
    
    member this.Interest() =
        [
            for tile in tiles do
                for pos in tile.Around() do
                    yield pos;
        ]
