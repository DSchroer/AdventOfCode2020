module Tests

open System
open Xunit
open Common.Regex
open Common.Files

type Instruction =
   | Memory of location: uint64 * value: uint64
   | Mask of mask: string

let applyMask (mask: string) value =
    let mutable result = value
    for (i, c) in mask.ToCharArray() |> Array.rev |> Array.indexed do
        match c with
        | 'X' -> ()
        | '0' -> result <- result &&& ~~~((uint64 1) <<< i)
        | '1' -> result <- result ||| ((uint64 1) <<< i)
        | _ -> failwith "Invalid mask char"
    result
    
let memoryMask (mask: string) value: uint64 seq =
    let mutable result = value
    let perms = pown 2 (mask |> Seq.filter ((=) 'X') |> Seq.length)
    seq {
        for perm in [0 ..perms] do
            let mutable xIndex = 0
            for (i, c) in mask.ToCharArray() |> Array.rev |> Array.indexed do
                match c with
                | 'X' ->
                    let pick = perm &&& (1 <<< xIndex)
                    xIndex <- xIndex + 1
                    if pick = 0 then result <- result &&& ~~~((uint64 1) <<< i)
                    else result <- result ||| ((uint64 1) <<< i)
                | '0' -> ()
                | '1' -> result <- result ||| ((uint64 1) <<< i)
                | _ -> failwith "Invalid mask char"
            yield result
    }
    
type Runtime() =
    let mutable storage = Map.empty<uint64, uint64>
    let mutable mask = ""
    
    member this.Exec inst =
        match inst with
        | Mask (m) -> mask <- m
        | Memory (loc, value) ->
            storage <- storage.Add(loc, (applyMask mask value))
    member this.Sum () =
        storage
        |> Map.toList
        |> List.map (fun (k, v) -> v)
        |> List.fold (fun a b -> a + int64 b) 0L
    
type RuntimeV2() =
    let mutable storage = Map.empty<uint64, uint64>
    let mutable mask = ""
    
    member this.Exec inst =
        match inst with
        | Mask (m) -> mask <- m
        | Memory (loc, value) ->
            for addr in memoryMask mask loc do
                storage <- storage.Add(addr, value)
    member this.Sum () =
        storage
        |> Map.toList
        |> List.map (fun (k, v) -> v)
        |> List.fold (fun a b -> a + int64 b) 0L
    
[<Fact>]
let ``Masks work`` () =
    let m1 = "1XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    Assert.Equal(uint64 64, applyMask m1 (uint64 0))


let parseInst line =
    match line with
    | Regex @"mask = (\w+)" [mask] -> Mask(mask = mask)
    | Regex @"mem\[(\d+)\] = (\d+)" [addr; value] -> Memory(location = uint64 addr, value = uint64 value)
    | _ -> failwithf "Invalid line %s" line

let input() =
    read "Instructions.txt" parseInst
    
let example() =
    let data = """
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"""
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Seq.map parseInst
    
[<Fact>]
let ``Can run example system`` () =
    let r = Runtime ()
    for i in example() do
        r.Exec i
    Assert.Equal(165L, r.Sum ())
    
[<Fact>]
let ``Can run docking system`` () =
    let r = Runtime ()
    for i in input() do
        r.Exec i
    Assert.Equal(342749663413L, r.Sum ())
   
    
[<Fact>]
let ``Memory masks work`` () =
    let m = "000000000000000000000000000000X1001X"
    let data = memoryMask m (uint64 42) |> Seq.toArray
    Assert.Equal(uint64 26, data.[0])
    Assert.Equal(uint64 27, data.[1])
    Assert.Equal(uint64 58, data.[2])
    Assert.Equal(uint64 59, data.[3])

let exampleV2() =
    let data = """
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
"""
    data.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> Seq.map parseInst

[<Fact>]
let ``Can run example system V2`` () =
    let r = RuntimeV2 ()
    for i in exampleV2() do
        r.Exec i
    Assert.Equal(208L, r.Sum ())

[<Fact>]
let ``Can run docking system V2`` () =
    let r = RuntimeV2 ()
    for i in input() do
        r.Exec i
    Assert.Equal(342749663413L, r.Sum ())