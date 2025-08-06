open Domino
open Domino.SimpleRules

let players = [ { Id = 0; Name = "Player #0" }; { Id = 1; Name = "Player #1" } ]
let rules = Impl() :> Rules<State, Action>

let rec prompt (playing: Playing) =
    printfn ""
    let player = playing.Active
    let actions = (Playing playing) |> rules.actions

    actions
    |> List.indexed
    |> List.iter (fun (index, (action)) -> printfn "[%d] [%02d] %A" index 0 action)

    printf "%s Action: " player.Name

    match System.Console.ReadLine() |> System.Int32.TryParse with
    | true, c when c < actions.Length -> actions |> List.item c
    | _ -> playing |> prompt

let rec run (playing: Playing) =
    printfn "%A" playing.Board
    printfn ""

    playing.Players
    |> Map.iter (fun player ps ->
        let tiles =
            ps.Tiles
            |> List.sortByDescending weight
            |> List.map (function
                | Tile(x, y) -> sprintf "[%d %d] " x y)
            |> System.String.Concat

        printfn "[%03d] %s %s" ps.Score player.Name tiles)

    playing
    |> prompt
    |> fun action -> (Playing playing) |> rules.play action
    |> function
        | Error err -> printfn "Error: %A" err
        | Ok state' ->
            match state' with
            | Playing playing' -> run playing'
            | Finished -> printfn "Game Finished"
            | _ -> printfn "Unexpected state"


players
|> rules.start
|> function
    | Ok state ->
        match state with
        | Playing playing -> run playing
        | _ -> printfn "Unexpected state after starting game"
    | Error err -> printfn "Error: %A" err
