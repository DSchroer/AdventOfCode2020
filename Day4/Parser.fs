module Day4.Parser

open Common.Regex
open Common.Files
open Day4.Passport

let parseField part =
    match part with
    | Regex @"(.+):(.+)" [ name; value ] -> Some { Name = name; Value = value }
    | _ -> None

let parsePassport (text: string) =
    text.Split(' ', '\n')
    |> Array.choose parseField
    |> Passport

let parsePassports (text: string) =
    text.Split("\n\n") |> Array.map parsePassport

let input () = readAll "Passports.txt" parsePassports
