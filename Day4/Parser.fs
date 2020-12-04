module Day4.Parser

open Common.Regex
open Common.Files
open Day4.Passport

let parsePassport(text: string) =
    text.Split(' ', '\n')
    |> Array.choose (fun part ->
        match part with
        | Regex @"(.+):(.+)" [name; value] -> Some {Name = name; Value = value}
        | _ -> None)
    |> Passport

let parsePassports(text: string) =
    text.Split("\n\n")
        |> Array.map parsePassport

let input() =
    readAll "Passports.txt" parsePassports
    