module Domino.SimpleRules

type Action =
    | StartGame of Player list
    | Lead of Tile
    | Attach of Tile * Tile

type Event =
    | GameStarted of Player list
    | TilesShuffled
    | RoundStarted

type Error = unit

type State =
    | Waiting
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
        | Waiting, StartGame players ->
            // XXX: check player count, unique names, etc.
            Ok [ GameStarted players; TilesShuffled; RoundStarted ]

        | _ -> Error()

let evolve: Evolve =
    fun state event ->
        printfn "Evolving state with event: %A" event

        match state, event with
        | Waiting, GameStarted players ->
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
            let state = Waiting

            let actions = players |> StartGame |> List.singleton

            (state, actions)
            ||> List.fold (fun state action ->
                printfn "Processing action: %A" action
                let events = state |> decide action

                match events with
                | Error _ -> state
                | Ok events -> (state, events) ||> List.fold evolve)
            |> Ok

        member _.actions state =
            match state with
            | Waiting -> []
            | Playing playing -> [] // Define actions for playing state
            | Finished -> [] // No actions available in finished state

        member _.play action state = Error "Not implemented"
