module Tests

open System
open Xunit
open Common.Files
open Common.Regex
open FSharp.Collections.ParallelSeq

type Range(start: int, finish: int) =
    member this.Start = start
    member this.End = finish
    member this.ValidFor x = x >= start && x <= finish
    

type Rule(name: string, range1: Range, range2: Range) =
    member this.Name = name
    member this.ValidFor x = range1.ValidFor x || range2.ValidFor x
    member this.ValidFor (all: int seq) =
        let invalid = all |> Seq.tryFind (fun f -> not (this.ValidFor f))
        match invalid with
        | Some _ -> false
        | None -> true
    override this.GetHashCode () = this.Name.GetHashCode()
    override this.Equals obj = this.Name = (obj :?> Rule).Name
    interface IComparable with
        member this.CompareTo obj = this.Name.CompareTo((obj :?> Rule).Name)

type Ticket(numbers: int seq) =
    let numList = numbers |> Seq.toList
    member this.Numbers = numbers
    member this.At n = numList.[n]
    member this.IsValid (rules: Rule seq) =
       numbers
       |> Seq.map (fun n -> rules |> Seq.map (fun r -> r.ValidFor n) |> Seq.reduce (||))
       |> Seq.reduce (&&)

let split (pattern: string) (line: string) =
    line.Split(pattern, StringSplitOptions.RemoveEmptyEntries)

let parseRules (line: string) =
    split "\n" line
    |> Seq.map (fun l ->
        match l with
        | Regex @"(.+): (\d+)-(\d+) or (\d+)-(\d+)" [ name; r1Min; r1Max; r2Min; r2Max ] ->
            Rule(name, Range(int r1Min, int r1Max), Range(int r2Min, int r2Max)))

let parseTickets line =
     split "\n" line
    |> Seq.skip 1
    |> Seq.map (fun l ->
        split "," l |> Seq.map int |> Ticket)

let parseInput () =
    readAll "Tickets.txt" (fun t ->
        let groups = split "\n\n" t

        (parseRules groups.[0], parseTickets groups.[1] |> Seq.head, parseTickets groups.[2]))

[<Fact>]
let ``Can locate invalid numbers`` () =
    let (rules, _, tickets) = parseInput()
    let numbers = tickets |> Seq.map (fun t -> t.Numbers |> Seq.toList) |> Seq.reduce (@)
    let invalidNumbers = numbers |> Seq.filter (fun n ->
        rules |> Seq.map (fun r -> not (r.ValidFor n)) |> Seq.reduce (&&)) |> Seq.sum
    Assert.Equal(32835, invalidNumbers)
    
[<Fact>]
let ``Can remove invalid rules`` () =
    let (rules, _, tickets) = parseInput()
    let validTickets = tickets |> Seq.filter (fun t -> t.IsValid rules)
    Assert.NotEmpty(validTickets)
    

[<Fact(Skip = "Takes too long")>]
let ``Can determine best rule`` () =
    let (rules, personal, tickets) = parseInput()
    
    let validTickets = tickets |> Seq.filter (fun t -> t.IsValid rules)
    let ruleSets = {0..(rules |> Seq.length) - 1}
                |> Seq.map (fun i -> 
                    let ticketValues = validTickets |> Seq.map (fun t -> t.At i)
                    
                    rules
                    |> Seq.filter (fun (r: Rule) -> (r.ValidFor ticketValues))
                    |> Seq.toArray)
                |> Seq.indexed
                |> Seq.toArray
                |> Seq.sortBy (fun (_, r) -> r.Length)
    
    let mutable known = Map.empty<int, Rule>
    let mutable seen = Set.empty<Rule>
    for (index, rules) in ruleSets do
        let rule = rules |> Array.find (fun r -> not (seen.Contains r))
        seen <- seen.Add(rule)
        known <- known.Add(index, rule)
        
    let total = known
                |> Map.toSeq
                |> Seq.filter (fun (_, r) -> r.Name.StartsWith("departure"))
                |> Seq.map (fun (i, _) -> int64 (personal.At i))
                |> Seq.reduce (*)
    
    Assert.Equal(514662805187L, total)
