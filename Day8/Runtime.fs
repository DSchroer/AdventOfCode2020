module Day8.Runtime

open Common

type Operator =
    | NOP
    | ACC
    | JMP

type Instruction = { Operator: Operator; Value: int }

type Program(instructions: Instruction list) =
    let mutable accumulator = 0
    let mutable programCounter = 0

    let execute instruction =
        match instruction with
        | { Operator = NOP } -> programCounter <- programCounter + 1
        | { Operator = JMP; Value = value } -> programCounter <- programCounter + value
        | { Operator = ACC; Value = value } ->
            accumulator <- accumulator + value
            programCounter <- programCounter + 1

    member this.Accumulator = accumulator
    member this.ProgramCounter = programCounter
    member this.Length = instructions.Length

    member this.Tick() =
        let instruction = instructions.[programCounter]
        (execute instruction)

let execute (program: Program) =
    let mutable pc: int list = []
    while not (pc |> List.contains(program.ProgramCounter) || program.ProgramCounter >= program.Length) do
        pc <- pc @ [program.ProgramCounter]
        program.Tick ()
    program
    
let pathInstruction (instruction: Instruction) =
    match instruction with
    | {Operator = NOP; Value = value} -> {Operator = JMP; Value = value}
    | {Operator = JMP; Value = value} -> {Operator = NOP; Value = value}
    | _ -> instruction

let patch (instructions: Instruction list) =
    [0..instructions.Length] |>
        List.choose (fun i ->
            let newInstructions = instructions |> List.mapi (fun index value -> if not (index = i) then value else pathInstruction value)
            let res = execute (newInstructions |> Program)
            if res.ProgramCounter = res.Length then Some res
            else None)

let parseInstruction (line: string) =
    let splits = line.Split(' ')
    let op = splits.[0]
    let value = splits.[1]
    match op with
    | "acc" -> {Operator = ACC; Value = value |> int}
    | "nop" -> {Operator = NOP; Value = value |> int}
    | "jmp" -> {Operator = JMP; Value = value |> int}
    | _ -> failwithf "Unsupported operation '%s'" op

let bootloader() =
    Files.read "Bootloader.txt" parseInstruction