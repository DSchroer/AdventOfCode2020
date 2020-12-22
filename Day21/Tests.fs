module Tests

open System
open Xunit
open Common.Regex
open Common.Files

type Recipe(ingredients: string list, allergens: string list) =
    member this.Ingredients = ingredients
    member this.Allergens = allergens
    
let parseRecipe text =
    let split (line: string) = line.Split([|' '; ','|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
    match text with
    | Regex @"^(.+)\(contains (.+)\)$" [ing; aler] -> Recipe(split ing, split aler)

let example() =
    let recipe = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""
    recipe.Split('\n') |> Array.map parseRecipe |> Array.toList

let input() =
    read "Ingredients.txt" parseRecipe |> Seq.toList

[<Fact>]
let ``Can find safe ingredients`` () =
    let recipes = input()
    
    let mutable lookup = Map.empty<string, Set<string>>
    for recipe in recipes do
        for allergen in recipe.Allergens do
            let existing = if lookup.ContainsKey allergen then lookup.[allergen] else Set(recipe.Ingredients)
            let nextSet = Set.intersect existing (Set(recipe.Ingredients))
            lookup <- lookup.Add(allergen, nextSet)
            
    let likelyAllergens = lookup |> Map.toList |> List.map (fun (k, v) -> v) |> List.reduce Set.union
            
    let safeIngredients = recipes
                         |> List.map (fun r -> r.Ingredients)
                         |> List.reduce (@)
                         |> List.filter (fun i -> not (likelyAllergens.Contains i))
                         
    Assert.Equal(2779, safeIngredients.Length)

[<Fact>]
let ``Can list all allergens`` () =
    let recipes = input()
    let mutable lookup = Map.empty<string, Set<string>>
    for recipe in recipes do
        for allergen in recipe.Allergens do
            let existing = if lookup.ContainsKey allergen then lookup.[allergen] else Set(recipe.Ingredients)
            let nextSet = Set.intersect existing (Set(recipe.Ingredients))
            lookup <- lookup.Add(allergen, nextSet)
            
    let mutable final = Map<string, string>([])
    let mutable res = lookup |> Map.map (fun k v -> v |> Set.toList) |> Map.toList
    while not res.IsEmpty do
        let (name, ingredientSet) = res |> List.find (fun (k, v) -> v.Length = 1)
        let ingredient =  ingredientSet.[0]
        final <- final.Add(name, ingredient)
        res <- res
               |> List.map (fun (k,v) -> (k, v |> List.filter (fun i -> i <> ingredient)))
               |> List.filter (fun (k, _) -> k <> name)
        ()
    
    let danger = final
                     |> Map.toList
                     |> List.sortBy (fun (k,_) -> k)
                     |> List.map (fun (_,v) -> v)
                     |> (fun list ->  String.Join(',', list))
                     
    Assert.Equal("lkv,lfcppl,jhsrjlj,jrhvk,zkls,qjltjd,xslr,rfpbpn", danger)