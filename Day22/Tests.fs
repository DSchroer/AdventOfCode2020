module Tests

open Day22
open Xunit

type Player = P1 | P2

type State = {P1: int list; P2: int list}
    with
    member this.WinP1() = {
        P1 = this.P1.Tail @ [this.P1.Head; this.P2.Head]
        P2 = this.P2.Tail
    }
    member this.WinP2() = {
        P1 = this.P1.Tail
        P2 = this.P2.Tail @ [this.P2.Head; this.P1.Head]
    }

type IGame =
    abstract HasWinner: bool
    abstract Winner: Unit -> Player
    abstract Deck: Player -> int list
    abstract NextRound: Unit -> IGame

let play (game: IGame) =
    let mutable round = game
    while not round.HasWinner do
        round <- round.NextRound ()
    round

type Game(state: State) =
    interface IGame with
        member this.HasWinner = state.P1.IsEmpty || state.P2.IsEmpty
        member this.Winner () = if state.P1.IsEmpty then P2 else P1
        member this.Deck player =
            match player with
            | P1 -> state.P1
            | P2 -> state.P2
        member this.NextRound() =
            if state.P1.Head > state.P2.Head
            then Game(state.WinP1()) :> IGame
            else Game(state.WinP2()) :> IGame

type RecursiveGame(state: State, history: Set<State>) =
    interface IGame with
        member this.HasWinner = history.Contains state || state.P1.IsEmpty || state.P2.IsEmpty
        member this.Winner () =
            if history.Contains state || state.P2.IsEmpty
            then P1
            else P2
        member this.Deck player =
            match player with
            | P1 -> state.P1
            | P2 -> state.P2
        
        member this.NextRound() =
            let p1Top = state.P1.Head
            let p2Top = state.P2.Head
            if state.P1.Tail.Length >= p1Top && state.P2.Tail.Length >= p2Top
            then
                let next = play (RecursiveGame({
                    P1 = state.P1.Tail.[0..p1Top - 1]
                    P2 = state.P2.Tail.[0..p2Top - 1]
                }, Set([])))
                let nextState = if next.Winner() = P1 then state.WinP1() else state.WinP2()
                RecursiveGame(nextState, history.Add(state)) :> IGame
            else
                let nextState = if state.P1.Head > state.P2.Head then state.WinP1() else state.WinP2()
                RecursiveGame(nextState, history.Add(state)) :> IGame
      
[<Fact>]
let ``Can complete game`` () =
    let game = Game({P1 = Decks.P1(); P2 = Decks.P2()})
    let finish = play game
    let sum = finish.Deck(finish.Winner()) |> List.rev |> List.mapi (fun i v -> (i + 1) * v) |> List.reduce (+)
    Assert.Equal(29764, sum)
    
[<Fact>]
let ``Can complete example recursive game`` () =
    let (p1, p2) = Decks.Ex()
    let game = RecursiveGame({P1 = p1; P2 = p2}, Set([]))
    let finish = play game
    let sum = finish.Deck(finish.Winner()) |> List.rev |> List.mapi (fun i v -> (i + 1) * v) |> List.reduce (+)
    Assert.Equal(291, sum)
    
[<Fact>]
let ``Can complete recursive game`` () =
    let game = RecursiveGame({P1 = Decks.P1(); P2 = Decks.P2()}, Set([]))
    let finish = play game
    let sum = finish.Deck(finish.Winner()) |> List.rev |> List.mapi (fun i v -> (i + 1) * v) |> List.reduce (+)
    Assert.Equal(32588, sum)
