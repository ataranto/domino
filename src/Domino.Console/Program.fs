open System

open Domino
open Domino.Board
open Domino.Events

let rec prompt player state =
    printfn ""

    let tiles =
        state.Players
        |> Map.find player
        |> fun ps -> ps.Tiles

    let actions =
        state.Board
        |> actions tiles
        |> List.map (fun action ->
            let score =
                match action with
                | Lead tile -> lead tile
                | Attach (tile, target) -> state.Board |> attach tile target
                |> score
            score, action
        )
        |> List.sortDescending

    actions
    |> List.indexed
    |> List.iter (fun (index, (score, action)) ->
        printfn "[%d] [%02d] %A" index score action
    )

    printfn "%s Action: " player.Name
    match Console.ReadLine() |> Int32.TryParse with
    | true, c when c < actions.Length -> actions |> List.item c |> snd
    | _                               -> state |> prompt player

let rec run events =
    let state =
        events
        |> List.fold apply zero

    printfn "\n--\n"
    printfn "%A" state.Board
    printfn ""

    state.Players
    |> Map.iter (fun player ps ->
        let tiles =
            ps.Tiles
            |> List.map (function Tile (x, y) -> sprintf "[%d %d] " x y)
            |> System.String.Concat

        printfn "[%03d] %s %s" ps.Score player.Name tiles
    )

    match state.Winner, state.Active with
    | None, Some player ->
        state
        |> prompt player
        |> fun action ->
            state
            |> execute action
            |> function
                | Error _    -> events
                | Ok events' -> events' |> List.append events
            |> run
    | _ ->
        events

[<EntryPoint>]
let main argv =
    printfn "== Domino =="

    let players = [
        { Id = 0; Name = "Player #0" }
        { Id = 1; Name = "Player #1" }
    ]

    players
    |> start
    |> run
    |> ignore

    0
