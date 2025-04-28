namespace Domino

type Rules<'State, 'Action> =
    abstract member start: Player list -> Result<'State, string>
    abstract member actions: 'State -> 'Action list
    abstract member play: 'Action -> 'State -> Result<'State, string>

module SimpleRules =
    type Action =
        | Lead of Tile
        | Attach of Tile * Tile

    type State =
        { Players: Map<Player, PlayerState>
          Active: Player
          Board: Tree
          Tiles: Tile list }

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

    let checkPlayerCount players =
        if List.length players >= 2 && List.length players <= 4 then
            Ok players
        else
            Error "Number of players must be between 2 and 4"

    let init players =
        let hands, boneyard = tiles |> List.ofSeq |> List.randomShuffle |> deal players
        let playerStates = hands |> Map.map (fun _ tiles -> { Tiles = tiles; Score = 0 })

        let active =
            playerStates
            |> Map.toList
            |> List.maxBy (fun (_, ps) -> ps.Tiles |> List.maxBy weight)
            |> fst

        { Players = playerStates
          Active = active
          Board = Empty
          Tiles = boneyard }

    let edges board =
        let rec loop result =
            function
            | Empty -> result
            | Node(tile, edges') ->
                edges'
                |> List.collect (function
                    | value, Empty -> (tile, value) :: result
                    | _, tree -> tree |> loop result)

        board |> loop List.empty

    let attach tile tree =
        let values =
            function
            | Tile(x, y) -> [ x; y ]

        let rec loop =
            function
            | Empty -> Node(tile, tile |> values |> List.map (fun value -> value, Empty))

        tree |> loop

    // transition to the next player's turn
    let turn state =
        let players = state.Players |> Map.keys

        let active =
            players
            |> Seq.head
            |> Seq.singleton
            |> Seq.append players
            |> Seq.pairwise
            |> Seq.pick (function
                | curr, next when curr = state.Active -> Some next
                | _ -> None)

        { state with Active = active }

    type Impl() =
        interface Rules<State, Action> with
            member _.start players =
                players |> checkPlayerCount |> Result.map init

            member _.actions state =
                let tiles = state.Players |> Map.find state.Active |> (fun ps -> ps.Tiles)

                match state.Board with
                | Empty -> tiles |> List.maxBy weight |> Lead |> List.singleton
                | node ->
                    node
                    |> edges
                    |> List.allPairs tiles
                    |> List.choose (function
                        | Tile(x, y), (tile, value) when x = value || y = value -> Some(Attach(Tile(x, y), tile))
                        | _ -> None)
                    |> List.distinct

            member this.play action state =
                let rules = this :> Rules<State, Action>

                if state |> rules.actions |> List.contains action |> not then
                    Error(
                        "Invalid action. Valid actions are: "
                        + (state |> rules.actions |> List.map (sprintf "%A") |> String.concat ", ")
                    )
                else
                    match action with
                    | Lead tile ->
                        { state with
                            Board = state.Board |> attach tile }
                        |> turn
                        |> Ok
