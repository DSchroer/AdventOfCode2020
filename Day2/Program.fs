module Day2.Program

open System
open Common
open Common.Regex

type Policy =
    { Lowest: int
      Highest: int
      Letter: char }

let parsePolicy line =
    match line with
    | Regex @"(\d+)-(\d+) (\w)" [ min; max; letter ] ->
        { Lowest = min |> int
          Highest = max |> int
          Letter = letter |> char }
    | _ -> raise (FormatException(line))

let parsePassword line =
    match line with
    | Regex @": (\w+)" [ password ] -> password
    | _ -> raise (FormatException(line))

let parse line =
    (parsePolicy line, parsePassword line)

let input() = Files.read "Passwords.txt" parse

let validPasswords policyFn =
    input()
    |>  Seq.filter (fun (policy, password) -> policyFn policy password)
    |>  Seq.length

let policy1 (policy: Policy) (password: string) =
    let count = Seq.filter (fun a -> a = policy.Letter) password |> Seq.length
    count >= policy.Lowest && count <= policy.Highest

let policy2 (policy: Policy) (password: string) =
    let a = password.[policy.Lowest - 1] = policy.Letter
    let b = password.[policy.Highest - 1] = policy.Letter
    a <> b
