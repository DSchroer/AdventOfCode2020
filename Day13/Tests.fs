module Tests

open System
open Xunit
open Day13.Bus
open Common.Files
open Common.Regex

let parseBus (index, text) =
    match text with
    | Regex @"(\d+)" [id] -> Some (Bus (int id, index) )
    | _ -> None

let parseBusLine (text: string) =
    text.Split(',') |> Array.indexed |> Array.choose parseBus |> Array.toSeq

let parseInput() =
    readAll "Table.txt" (fun text ->
        let lines = text.Split('\n', StringSplitOptions.RemoveEmptyEntries)
        let target = lines.[0] |> int
        let buses = parseBusLine lines.[1]
        (target, buses))

[<Fact>]
let ``Can determine next bus`` () =
    let (target, buses) = parseInput()
    let next = buses |> Seq.minBy (fun b -> b.DepartsAfter target)
    let departure = next.DepartsAfter target
    let waitTime = int64 (departure - target)
    Assert.Equal(370L, (next.Id * (waitTime)))
    
let inStationOffset (time: int64) (bus: Bus) =
    ((time + bus.Index) % bus.Id) = 0L
    

let occurence (buses: Bus seq) =
    let ids = buses |> Seq.map (fun b -> b.Id)
    let final = ids |> Seq.reduce (*)
    let largest = Seq.maxBy (fun (b: Bus) -> b.Id) buses
    {-largest.Index .. largest.Id .. final} |>
        Seq.find (fun time ->
            let busTimes = buses |> Seq.map (fun bus -> inStationOffset time bus) |> Seq.toArray
            busTimes |> Seq.reduce (&&))

[<Fact>]
let ``Can determine when the periods align``() =
    Assert.Equal(3417L, occurence (parseBusLine "17,x,13,19"))
    Assert.Equal(754018L, occurence (parseBusLine "67,7,59,61"))
    Assert.Equal(779210L, occurence (parseBusLine "67,x,7,59,61"))
    Assert.Equal(1261476L, occurence (parseBusLine "67,7,x,59,61"))
    Assert.Equal(1202161486L, occurence (parseBusLine "1789,37,47,1889"))

let rec gcd a b =
  if b = 0L 
    then abs a
  else gcd b (a % b)

let MI n g =
  let rec fN n i g e l a =
    match e with
    | 0L -> g
    | _ -> let o = n/e
           fN e l a (n-o*e) (i-o*l) (g-o*a) 
  (n+(fN n 1L 0L g 0L 1L))%n

let CD n g =
  match Seq.fold(fun n g->if (gcd n g)=1L then n*g else 0L) 1L g with
  |0L -> None
  |fN-> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(MI g ((fN/g)%g))) 0L n g)%fN)

[<Fact>]
let ``Can determine when the periods align for input``() =
    let (_, buses) = parseInput()
    let ids = buses |> Seq.map (fun b -> b.Id)
    let indexs = buses |> Seq.map (fun b -> - b.Index)
    let period =  Seq.reduce (*) ids
    let cd = (CD indexs ids)
    Assert.Equal(894954360381385L, cd.Value + period)