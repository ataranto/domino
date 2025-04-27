namespace Domino

type Rules<'State> =
    abstract member start: Player list -> Result<'State, string>

module SimpleRules =
    type State =
        { Players: Map<Player, PlayerState>
          Active: Player
          Board: Tree<Tile>
          Tiles: Tile list }

    let tiles =
        let maxValue = 6

        [ for x in 0..maxValue do
              for y in x..maxValue do
                  yield Tile(x, y) ]
        |> set

    let weight =
        function
        | Tile(x, y) -> x = y, x + y, max x y

    let checkPlayerCount players =
        if List.length players >= 2 && List.length players <= 4 then
            Ok players
        else
            Error "Number of players must be between 2 and 4"

    let init players =
        let handSize = 7
        let shuffledTiles = tiles |> List.ofSeq |> List.randomShuffle

        let playerStates =
            shuffledTiles
            |> List.chunkBySize handSize
            |> List.take (players |> List.length)
            |> List.zip players
            |> List.map (fun (player, tiles) -> player, { Tiles = tiles; Score = 0 })
            |> Map.ofList

        let active =
            playerStates
            |> Map.toList
            |> List.maxBy (fun (_, ps) -> ps.Tiles |> List.maxBy weight)
            |> fst

        let boneyard = shuffledTiles |> List.skip (players |> List.length |> (*) handSize)

        { Players = playerStates
          Active = active
          Board = Empty
          Tiles = boneyard }

    type Impl() =
        interface Rules<State> with
            member _.start players =
                players |> checkPlayerCount |> Result.map init
