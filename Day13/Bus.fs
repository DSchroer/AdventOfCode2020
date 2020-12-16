module Day13.Bus

open System

type Bus(id: int, index: int) =
    let period = id
    
    member this.Id = int64 id
    member this.Index = int64 index
    member this.DepartsAfter(time: int) =
        let times =  (int (Math.Ceiling ((float time) / (float period))))
        times * period