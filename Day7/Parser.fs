module Day7.Parser

open Common
open Common.Regex
open Rules

let parseContain(line: string) =
    match line with
    | Regex @"(\d+) (.+) bags?" [amount; name] -> Some (name, (int amount))
    | _ -> None
    
let parseContainList(parts: string seq) =
    parts
    |> Seq.choose parseContain
    
let parseRule(text: string) =
    match text with
    | Regex @"(.+) bags contain (.+)." [name; contain] -> Rule(name, (contain.Split(',') |> parseContainList |> Map<string, int>))
    | _ -> failwith "Can not parse bag"

let parseAllRules() =
    Files.read "Rules.txt" parseRule