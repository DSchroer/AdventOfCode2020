module Day4.Passport

open Common.Regex

type Field = { Name: string; Value: string }

type Passport(fields: Field array) =
    member this.Fields = fields

    member this.FieldNames =
        Set.ofArray (fields |> Array.map (fun a -> a.Name))

let requiredFields =
    [ "byr"
      "iyr"
      "eyr"
      "hgt"
      "hcl"
      "ecl"
      "pid" ]

let isValid (passport: Passport) =
    requiredFields
    |> List.fold (fun a b -> a && passport.FieldNames.Contains(b)) true

let isValidField (field: Field) =
    let between value min max =
        let current = value |> int
        current >= min && current <= max

    match field with
    | { Name = "byr" } -> between field.Value 1920 2002
    | { Name = "iyr" } -> between field.Value 2010 2020
    | { Name = "eyr" } -> between field.Value 2020 2030
    | { Name = "hgt" } ->
        match field.Value with
        | Regex @"^(\d+)cm$" [ height ] -> between height 150 193
        | Regex @"^(\d+)in$" [ height ] -> between height 59 76
        | _ -> false
    | { Name = "hcl" } ->
        match field.Value with
        | Regex @"^#[0-9a-f]{6}$" _ -> true
        | _ -> false
    | { Name = "ecl" } ->
        match field.Value with
        | Regex @"^amb|blu|brn|gry|grn|hzl|oth$" _ -> true
        | _ -> false
    | { Name = "pid" } ->
        match field.Value with
        | Regex @"^\d{9}$" _ -> true
        | _ -> false
    | _ -> true

let isStrictValid (passport: Passport) =
    isValid passport
    && passport.Fields
    |> Array.fold (fun a b -> a && isValidField b) true
