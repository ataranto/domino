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

type Error = unit

type State =
    | Waiting of Player list
    | Starting of Starting
    | Playing of Playing
    | Finished

and Starting = { Players: Map<Player, PlayerState> }

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
            Ok [ GameStarted; TilesShuffled ]

        | _ -> Error()

let evolve: Evolve =
    fun state event ->
        printfn "--- Evolving state with event: %A" event

        match state, event with
        | Waiting players, PlayerAdded player -> Waiting(player :: players)
        | Waiting players, GameStarted ->
            Starting { Players = players |> List.map (fun p -> p, { Tiles = []; Score = 0 }) |> Map.ofList }
        // Playing
        //     { Players = players |> List.map (fun p -> p, { Tiles = []; Score = 0 }) |> Map.ofList
        //       Active = players |> List.head
        //       Board = Empty
        //       Tiles = [] }
        | Starting starting, TilesShuffled ->
            let hands, boneyard =
                tiles
                |> List.ofSeq
                |> List.randomShuffle
                |> deal (starting.Players |> Map.keys |> List.ofSeq)

            let players =
                starting.Players
                |> Map.map (fun player _ -> { Tiles = hands.[player]; Score = 0 })

            let active =
                players
                |> Map.toList
                |> List.maxBy (fun (_, ps) -> ps.Tiles |> List.map weight |> List.max)
                |> fst

            Playing
                { Players = players
                  Active = active
                  Board = Empty
                  Tiles = boneyard }

        | _ -> failwithf "Unexpected event %A in state %A" event state

type Impl() =
    interface Rules<State, Action> with
        member _.start players =
            let state = Waiting []
            let actions = (players |> List.map (fun p -> AddPlayer p)) @ [ StartGame ]

            let folder result action =
                match result with
                | Error err -> Error err
                | Ok state ->
                    match decide action state with
                    | Error _ -> Error "bad list of players"
                    | Ok events -> Ok((state, events) ||> List.fold evolve)

            (Ok state, actions) ||> List.fold folder

        member _.actions state =
            match state with
            | Waiting players -> [ StartGame ]
            | Starting starting -> [] // Define actions for starting state
            | Playing playing -> [] // Define actions for playing state
            | Finished -> [] // No actions available in finished state

        member _.play action state =
            match state |> decide action with
            | Error err -> Error "Invalid action"
            | Ok events -> Ok((state, events) ||> List.fold evolve)
