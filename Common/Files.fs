module Common.Files

open System.IO

let private sourceDir = "../../../"

let read (file: string) (parser: string -> 'T)  =
      File.ReadAllLines(Path.Join(sourceDir, file))
        |> Seq.map parser

let readAll (file: string) (parser: string -> 'T)  =
      File.ReadAllText(Path.Join(sourceDir, file))
        |> parser