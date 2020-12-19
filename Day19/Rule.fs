module Day19.Rule

open System
open Common.Regex

type Rule =
    | Refs of int list
    | Join of Rule * Rule
    | Value of char
    | Rec11 of int * int
    | Rec8 of int

type Options(strs: string list) =
    member this.Strs = strs
    member this.Matches text = (strs |> List.tryFind (fun t -> t = text)).IsSome
    member this.Starts (text: string) = (strs |> List.tryFind (fun t -> text.StartsWith(t))).IsSome
    member this.Length = strs.[0].Length
    static member (&&&) (a: Options, b: Options) =
        Options([
            for aString in a.Strs do
                for bString in b.Strs do
                    yield aString + bString
        ])
    static member (|||) (a: Options, b: Options) =
        Options(a.Strs @ b.Strs)
    static member (|^|) (a: Options, text: string) =
        Options(a.Strs |> List.filter (fun s -> text.IndexOf s <> -1))

type RuleSet(rules: Map<int, Rule>) =
    let printFor (text: string) =
        let rec print rule =
            match rule with
            | Value c -> Options([string c])
            | Join (l, r) -> (print l) ||| (print r)
            | Refs list -> list |> List.map (fun i -> print rules.[i]) |> List.fold (&&&) (Options [""])
            | Rec8 i ->
                let part = print (Refs [i]) |^| text
                let mutable d1 = part
                for _ in [0..4] do
                    d1 <- d1 ||| ((d1 &&& part) |^| text)
                d1
            | Rec11 (left, right) ->
                let l = print (Refs [left]) |^| text
                let r = print (Refs [right]) |^| text
                let mutable d1 = (l &&& r)
                for _ in [0..4] do
                    d1 <- d1 ||| ((l &&& d1 &&& r) |^| text)
                d1 
        print
    
    member this.Print text index = printFor text rules.[index]
    
    member this.Matches (id: int) (text: string) =

        let rec matches rule (text: string) =
            match rule with
            | Value c -> if text.Length >=1 && text.[0] = c then Some 1 else None

            | Join (l, r) ->
                let left = matches l text

                if left.IsSome then
                    left
                else
                    let right = matches r text
                    if right.IsSome then right else None

            | Refs list ->
               list |> List.fold (fun state id ->
                   match state with
                   | None -> None
                   | Some pos ->
                       if text.Length < pos then None
                       else
                           let m = matches rules.[id] (text.Substring(pos))
                           if m.IsSome then Some (pos + m.Value)
                           else None
                   ) (Some 0)
               
//            | Rec8 (rule) ->
//                let l = print (Refs [rule])
//                let len = l.Length
//                if l.Starts text
//                then Some len
//                else None
//            
//            | Rec11 (left, right) ->
//                let l = print (Refs [left])
//                let r = print (Refs [right])
//                let mutable total = l &&& r
//                None
                

        match (matches rules.[id] text) with
        | Some n -> text.Length = n
        | None -> false



let parseRule line =
    let parseRefs (txt: string) = txt.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Array.toList |> Refs
    
    try
        match line with
        | Regex "(\d+): \"(\w)\"" [id; c] -> (int id, Value c.[0])
        | Regex @"(\d+): (.+) \| (.+)" [id; left; right] -> (int id,Join (parseRefs left, parseRefs right))
        | Regex @"(\d+): (.+)" [id; refs] -> (int id, parseRefs refs)
    with
    | _ -> failwith line
