module Domino.Tests.Events

open Xunit
open FsUnit.Xunit

open Domino
open Events

module zero =
    [<Fact>]
    let ``the Players map is empty`` () =
        zero
        |> fun state -> state.Players
        |> should be FsUnit.CustomMatchers.Empty

    [<Fact>]
    let ``the Active player is None`` () =
        zero
        |> fun state -> state.Active
        |> should equal None

    [<Fact>]
    let ``the zero state Winner optional value is None`` () =
        zero
        |> fun state -> state.Winner
        |> should equal None

    [<Fact>]
    let ``the zero state Board value is Empty`` () =
        zero
        |> fun state -> state.Board
        |> should equal Empty

    [<Fact>]
    let ``the zero state Bonyard value is an empty list`` () =
        zero
        |> fun state -> state.Boneyard
        |> should be FsUnit.CustomMatchers.Empty

module integration =
    let players =
        Seq.initInfinite id
        |> Seq.map (fun id -> { Id = id; Name = sprintf "Player #%d" id})

    let rec run events =
        let state = events |> List.fold apply zero

        match state.Winner, state.Active with
        | Some winner, _ ->
            winner, events
        | None, Some player ->
            let tiles =
                state.Players
                |> Map.find player
                |> fun ps -> ps.Tiles
            let score = function
                | Lead tile ->
                    Board.lead tile |> Board.score
                | Attach (tile, target) ->
                    state.Board |> Board.attach tile target |> Board.score
            let action =
                state.Board
                |> Board.actions tiles
                |> List.maxBy score
            state
            |> execute action
            |> function
                | Error _ -> failwith "execute error"
                | Ok es   -> (events @ es) |> run
        | _ ->
            failwith "unknown state"

    [<Fact>]
    let ``a 2 player game can be completed and replayed`` () =
        let winner, events =
            players
            |> Seq.take 2
            |> Seq.toList
            |> start
            |> run

        printfn "\nwinner 2: %A" winner.Name
        printfn "events  : %d" events.Length

        events
        |> List.fold apply zero
        |> function
            | { Winner = Some player } -> player |> should equal winner
            | _                        -> failwith "mismatch"

    [<Fact>]
    let ``a 4 player game can be completed and replayed`` () =
        let winner, events =
            players
            |> Seq.take 4
            |> Seq.toList
            |> start
            |> run

        printfn "\nwinner 4: %A" winner.Name
        printfn "events  : %d" events.Length

        events
        |> List.fold apply zero
        |> function
            | { Winner = Some player } -> player |> should equal winner
            | _                        -> failwith "mismatch"
