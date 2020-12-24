module Tests

open Xunit
open FParsec
open Common.Files

let ws = spaces
let str s = pstring s

let listBetweenStrings sOpen sClose pElement =
    between (str sOpen) (str sClose) pElement
            
let grammar =
    let opp = OperatorPrecedenceParser<int64, unit, unit>()
    
    let expr = opp.ExpressionParser
    let term = pint64 <|> between (str "(") (str ")") expr
    opp.TermParser <- term
    
    opp.AddOperator(InfixOperator("+", ws, 2, Associativity.Left, fun a b -> a + b))
    opp.AddOperator(InfixOperator("*", ws, 1, Associativity.Left, fun a b -> a * b))

    expr

let tokenize input =
    let trimmed = System.Text.RegularExpressions.Regex.Replace(input, @"\s+", "")
    match run grammar trimmed with
    | Success (res, _, _) -> res
    | Failure (err, _, _) -> failwith err  
    
[<Fact>]
let ``Can run``() =
    let res = tokenize "1 + 2 * 3 + 4 * 5 + 6"
    Assert.Equal(231L, res)
    
[<Fact>]
let ``Can run homework``() =
    let lines = read "Homework.txt" tokenize
    Assert.Equal(122438593522757L, lines |> Seq.sum)