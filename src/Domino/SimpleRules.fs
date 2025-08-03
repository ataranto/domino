module Domino.SimpleRules

type Action =
    | AddPlayer of Player
    | StartGame
    | Lead of Tile
    | Attach of Tile * Tile

type Event =
    | PlayerAdded of Player
    | GameStarted
    | TilesShuffled
    | RoundStarted

type Error = unit

type State =
    | Waiting of Waiting
    | Playing of Playing
    | Finished

and Waiting = { Players: Set<Player> }

and Playing =
    { Players: Map<Player, PlayerState>
      Active: Player
      Board: Tree
      Tiles: Tile list }

type Decide = Action -> State -> Result<Event list, Error>
type Evolve = State -> Event -> State

let handSize = 7

let tiles =
    let maxValue = 6

    [ for x in 0..maxValue do
          for y in x..maxValue do
              yield Tile(x, y) ]
    |> set

let weight =
    function
    | Tile(x, y) -> x = y, x + y, max x y

let deal players tiles =
    let hands =
        tiles
        |> List.chunkBySize handSize
        |> List.take (players |> List.length)
        |> List.zip players
        |> List.map (fun (player, tiles) -> player, tiles)
        |> Map.ofList

    hands, tiles |> List.skip (players |> List.length |> (*) handSize)


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

            printfn "Starting game with players: %A" (players |> Map.keys)

            Playing
                { Players = players
                  Active = waiting.Players |> Set.toList |> List.head
                  Board = Empty
                  Tiles = [] }

        | Playing playing, TilesShuffled ->
            let hands, boneyard =
                tiles
                |> List.ofSeq
                |> List.randomShuffle
                |> deal (playing.Players |> Map.keys |> List.ofSeq)

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
                |> List.maxBy (fun (_, ps) -> ps.Tiles |> List.map weight |> List.max)
                |> fst

            Playing { playing with Active = active }

        | _ -> failwithf "Unexpected event %A in state %A" event state

type Impl() =
    interface Rules<State, Action> with
        member _.start players =
            let state = { Players = Set.empty } |> Waiting
            let actions = players |> List.map AddPlayer

            printfn "Actions to start: %A" actions

            (state, actions)
            ||> List.fold (fun state action ->
                printfn "Processing action: %A" action
                let events = state |> decide action

                match events with
                | Error _ -> state
                | Ok events -> (state, events) ||> List.fold evolve)
            |> fun state ->
                // use evolve to start the game
                GameStarted |> evolve state |> Ok

        member _.actions state =
            match state with
            | Waiting waiting -> []
            | Playing playing -> [] // Define actions for playing state
            | Finished -> [] // No actions available in finished state

        member _.play action state = Error "Not implemented"
