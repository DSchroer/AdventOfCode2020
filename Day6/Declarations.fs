module Day6.Declarations

open Common.Files

let singleAnswer (text: string) = 
    text
    |> Set.ofSeq
 
let anyoneAnswered (text: string) =
    text.Split('\n')
    |> Array.map singleAnswer
    |> Seq.reduce Set.union

let everyoneAnswered (text: string) =
    text.Trim().Split('\n')
    |> Array.map singleAnswer
    |> Seq.reduce Set.intersect

let parseDeclarations (text: string) (parser: string -> char Set) =
    text.Split("\n\n")
    |> Array.map parser

let input() =
    readAll "Declarations.txt"  parseDeclarations