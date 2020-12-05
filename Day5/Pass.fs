module Day5.Pass

open System

let private upper (min, max) =
    (((min + max + 1) / 2), max)

let private lower (min, max) =
    (min, ((min + max) / 2))

let rec private parseRow (min, max) code =
    if min = max then min
    else
        match Array.head code with
        | 'B' -> parseRow (upper (min, max)) (Array.tail code)
        | 'F' -> parseRow (lower (min, max)) (Array.tail code)
        | _ -> raise (ArgumentException(string code))
    
let rec private parseSeat (min, max) code =
    if min = max then min
    else
        match Array.head code with
        | 'R' -> parseSeat (upper (min, max)) (Array.tail code)
        | 'L' -> parseSeat (lower (min, max)) (Array.tail code)
        | _ -> raise (ArgumentException(string code))

type Pass = {SeatId: int}

type Pass with
    static member (+) (first: Pass, second: Pass) =
        {SeatId = first.SeatId + second.SeatId}
    static member get_One() =
        {SeatId = 1}

let parsePass (code: string) =
    let row = parseRow (0, 127) (code.[0 .. 6].ToCharArray())
    let seat = parseSeat (0,7) (code.[7 ..].ToCharArray())
    {SeatId = row * 8 + seat}
    
let passes() =
    Common.Files.read "Passes.txt" parsePass
    
let myPass() =
    let allPasses = passes()
    let firstId = allPasses |> Seq.min
    let lastId = allPasses |> Seq.max
    let passSet = allPasses |> Set.ofSeq
    seq { firstId .. lastId } |> Seq.find (fun pass -> not (passSet.Contains(pass)) )