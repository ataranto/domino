namespace Domino

type Player = {
    Id: int
    Name: string
}

type PlayerState = {
    Tiles: Tile list
    Score: int
}

type State = {
    Players: Map<Player, PlayerState>
    Active: Player option
    Winner: Player option
    Board: Tree
    Boneyard: Tile list
}

type Event =
    | GameStarted of Player list
    | TilesShuffled of Tile list
    | TurnStarted of Player
    | TileDrawn of Player * Tile
    | TurnBlocked of Player
    | TilePlayed of Player * Action * Points: int
    | Domino of Player * Points: int
    | Blocked of Player * Points: int
    | GameWon of Player

module Events =
    let private round n x =
        let dn, dx = decimal n, decimal x
        System.Math.Round(dx / dn, 0) * dn |> int

    let private shuffle xs =
        let random = System.Random()
        List.sortBy (fun _ -> random.Next()) xs

    let private next x xs =
        xs
        |> List.windowed 2
        |> List.tryPick (function
            | [left; right] when x = left -> Some right
            | _                           -> None
        )
        |> function
            | Some (x) -> x
            | _        -> xs |> List.head

    // http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list
    let rec private remove x = function
        | x'::xs when x' = x -> xs
        | x'::xs             -> x'::remove x xs
        | []                 -> []

    let zero = {
        Players = Map.empty
        Active = None
        Winner = None
        Board = Empty
        Boneyard = List.empty
    }

    let update player f state =
        state.Players
        |> Map.find player
        |> fun ps -> state.Players |> Map.add player (ps |> f)

    module private Apply =
        let start players =
            let create player =
                player, { Tiles = List.empty; Score = 0 }
            { zero with
                Players = players |> List.map create |> Map.ofList
            }

        let deal tiles state =
            { state with
                Board = Empty
                Players =
                    tiles
                    |> List.chunkBySize 7
                    |> List.take (state.Players |> Map.count)
                    |> List.zip (state.Players |> Map.toList)
                    |> List.map (fun ((player, ps), tiles) ->
                        player, { ps with Tiles = tiles }
                    )
                    |> Map.ofList
                Boneyard =
                    tiles
                    |> List.skip (state.Players |> Map.count |> (*) 7)
            }

        let draw player tile state =
            let players =
                state
                |> update player (fun ps -> { ps with Tiles = tile::ps.Tiles })
            { state with
                Boneyard = state.Boneyard |> remove tile
                Players = players
            }

        let play player action points state =
            let tile, board =
                match action with
                | Lead tile ->
                    tile, tile |> Board.lead
                | Attach (tile, target) ->
                    tile, state.Board |> Board.attach tile target
            let players =
                state
                |> update player (fun ps ->
                    { ps with
                        Tiles = ps.Tiles |> remove tile
                        Score = ps.Score + points
                    }
                )

            { state with
                Board = board
                Players = players
            }

        let tally player points state =
            let players =
                state
                |> update player (fun ps ->
                    { ps with Score = ps.Score + points }
                )
            { state with Players = players }

    let apply state = function
        | GameStarted players ->
            players |> Apply.start
        | TilesShuffled tiles ->
            state |> Apply.deal tiles
        | TurnStarted player ->
            { state with Active = Some player }
        | TileDrawn (player, tile) ->
            state |> Apply.draw player tile
        | TurnBlocked _ ->
            state
        | TilePlayed (player, action, points) ->
            state |> Apply.play player action points
        | Domino (player, points)
        | Blocked (player, points) ->
            state |> Apply.tally player points
        | GameWon player ->
            { state with Winner = Some player }

    let win player state =
        state.Players
        |> Map.find player
        |> function
            | { Score = score } when score >= 150 ->
                Some (GameWon player)
            | _ ->
                None

    let total player state =
        state.Players
        |> Map.map (fun _ { Tiles = tiles } ->
            tiles |> List.collect Tile.values |> List.sum
        )
        |> Map.remove player
        |> Map.toList
        |> List.sumBy snd
        |> round 5

    let domino player state =
        let tiles =
            state.Players
            |> Map.find player
            |> function { Tiles = tiles } -> tiles
        match tiles with
        | [] -> Domino (player, state |> total player) |> Some
        | _  -> None

    let blocked player state =
        let actions =
            state.Players
            |> Map.toList
            |> List.map (fun (_, ps) -> ps.Tiles)
            |> List.collect (fun tiles -> state.Board |> Board.actions tiles)
        match actions with
        | [] -> Blocked (player, state |> total player) |> Some
        | _  -> None

    let tilesShuffled () =
        Tile.tiles 6
        |> shuffle
        |> TilesShuffled

    let firstTurn state =
        let weight = function
            | Tile (x, y) -> x = y, x + y, max x y
        state.Players
        |> Map.toList
        |> List.maxBy (fun (_, ps) -> ps.Tiles |> List.maxBy weight)
        |> fst
        |> TurnStarted

    let nextTurn player state =
        state.Players
        |> Map.toList
        |> List.map fst
        |> next player
        |> TurnStarted

    let sequence state event =
        let rec loop state events =
            let next event =
                event::events
                |> loop (event |> apply state)

            match events with
            | GameStarted _::_ ->
                () |> tilesShuffled |> next
            | [TilesShuffled _; GameStarted _] ->
                state |> firstTurn |> next
            | TurnStarted player::_
            | TileDrawn (player, _)::_->
                let tiles =
                    state.Players
                    |> Map.find player
                    |> function { Tiles = tiles } -> tiles
                let actions =
                    state.Board
                    |> Board.actions tiles
                match actions, state.Boneyard with
                | [], [] -> player |> TurnBlocked |> next
                | [], _  -> TileDrawn (player, state.Boneyard |> List.head) |> next
                | _      -> events
            | TurnBlocked player::_ ->
                state |> nextTurn player |> next
            | TilePlayed (player, _, _)::_ ->
                [win; domino; blocked]
                |> List.tryPick (fun f -> state |> f player)
                |> function
                    | Some event -> event
                    | None       -> state |> nextTurn player
                |> next
            | Domino (player, _)::_
            | Blocked (player, _)::_ ->
                match state |> win player with
                | Some event -> event
                | None       -> () |> tilesShuffled
                |> next
            | _ ->
                events

        event
        |> List.singleton
        |> loop (event |> apply state)
        |> List.rev

    let start players =
        GameStarted players
        |> sequence zero

    let execute action state =
        match state.Active with
        | None ->
            Error "game not started"
        | Some player ->
            let event =
                match action with
                | Lead tile ->
                    TilePlayed (player, action, Board.lead tile |> Board.score)
                | Attach (tile, target) ->
                    TilePlayed (player, action, state.Board |> Board.attach tile target |> Board.score)

            event
            |> sequence state
            |> Ok
