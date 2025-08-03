module Domino.SimpleRules

type Action =
    | AddPlayer of Player
    | StartGame

type Event =
    | PlayerAdded of Player
    | GameStarted
    | TilesShuffled
    | RoundStarted

type Error = unit

type State =
    | Waiting of Waiting
    | Playing of Playing
    | Finshed

and Waiting = { Players: Set<Player> }

and Playing =
    { Players: Map<Player, PlayerState>
      Active: Player
      Board: Tree
      Tiles: Tile list }

type Decide = Action -> State -> Result<Event list, Error>
type Evolve = State -> Event -> State

let decide: Decide =
    fun action state ->
        match state, action with
        | Waiting waiting, AddPlayer player ->
            match waiting.Players |> Set.contains player with
            | true -> Error()
            | false -> Ok [ PlayerAdded player ]

        | Waiting waiting, StartGame -> Ok [ GameStarted; TilesShuffled; RoundStarted ]

        | _ -> Error()

let evolve: Evolve =
    fun state event ->
        match state, event with
        | Waiting waiting, PlayerAdded player ->
            Waiting
                { waiting with
                    Players = waiting.Players |> Set.add player }

        | Waiting waiting, GameStarted ->
            let players =
                waiting.Players
                |> Set.toList
                |> List.map (fun p -> p, { Tiles = []; Score = 0 })
                |> Map.ofList

            Playing
                { Players = players
                  Active = waiting.Players |> Set.toList |> List.head
                  Board = Empty
                  Tiles = [] }

        | Playing playing, TilesShuffled ->
            let hands, boneyard =
                OldSimpleRules.tiles
                |> List.ofSeq
                |> List.randomShuffle
                |> OldSimpleRules.deal (playing.Players |> Map.keys |> List.ofSeq)

            let players =
                playing.Players
                |> Map.map (fun player _ -> { Tiles = hands.[player]; Score = 0 })

            Playing
                { playing with
                    Players = players
                    Tiles = boneyard }

        | Playing playing, RoundStarted ->
            let active =
                playing.Players
                |> Map.toList
                |> List.maxBy (fun (_, ps) -> ps.Tiles |> List.map OldSimpleRules.weight |> List.max)
                |> fst

            Playing { playing with Active = active }

        | _ -> failwithf "Unexpected event %A in state %A" event state
