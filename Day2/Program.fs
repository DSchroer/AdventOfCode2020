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

let input = Files.read "Passwords.txt" parse

let validate (policy: Policy) (password: string) =
    let count = Seq.filter (fun a -> a = policy.Letter) password |> Seq.length
    count >= policy.Lowest && count <= policy.Highest

let validPasswords =
    input
    |>  Seq.filter (fun (policy, password) -> validate policy password)

[<EntryPoint>]
let main _ =
    let results = Seq.length validPasswords
    printfn "%d" results
    0 // return an integer exit code
