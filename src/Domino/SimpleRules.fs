module Domino.SimpleRules

type Action =
    | AddPlayer of Player
    | StartGame
    | Lead of Tile
    | Attach of Tile * Tile

type Event =
    | GameStarted
    | PlayerAdded of Player
    | TilesShuffled
    | RoundStarted

type Error = unit

type State =
    | Waiting of Player list
    | Playing of Playing
    | Finished

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
        | Waiting players, AddPlayer player -> Ok [ PlayerAdded player ]
        | Waiting [], StartGame -> Error()
        | Waiting players, StartGame ->
            // XXX: check player count, unique names, etc.
            Ok [ GameStarted; TilesShuffled; RoundStarted ]

        | _ -> Error()

let evolve: Evolve =
    fun state event ->
        printfn "Evolving state with event: %A" event

        match state, event with
        | Waiting players, PlayerAdded player ->
            printfn "Adding player: %A" player
            Waiting (player :: players)
        | Waiting [], GameStarted -> failwith "Cannot start a game with no players"
        | Waiting players, GameStarted ->
            printfn "Starting game with players: %A" players

            Playing
                { Players = players |> List.map (fun p -> p, { Tiles = []; Score = 0 }) |> Map.ofList
                  Active = players |> List.head
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
            Ok(Waiting [])

        member _.actions state =
            match state with
            | Waiting players -> [ StartGame ]
            | Playing playing -> [] // Define actions for playing state
            | Finished -> [] // No actions available in finished state

        member _.play action state =
            match state |> decide action with
            | Error err -> Error "Invalid action"
            | Ok events -> Ok((state, events) ||> List.fold evolve)
