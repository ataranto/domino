namespace Domino

type Rules<'State> =
    abstract member start: Player list -> Result<'State, string>


module SimpleRules =
    type State =
        { Players: Map<Player, PlayerState>
          Active: Player
          Board: Tree<Tile>
          Tiles: Tile list }

    let checkPlayerCount players =
        if List.length players >= 2 && List.length players <= 4 then
            Ok players
        else
            Error "Number of players must be between 2 and 4"

    let init players =
        let max = 6
        let handSize = 7

        let allTiles =
            [ for x in 0..6 do
                  for y in x..6 do
                      yield Tile(x, y) ]
            |> List.randomShuffle

        let playerStates =
            allTiles
            |> List.chunkBySize handSize
            |> List.take (players |> List.length)
            |> List.zip players
            |> List.map (fun (player, tiles) -> player, { Tiles = tiles; Score = 0 })
            |> Map.ofList

        let tiles = allTiles |> List.skip (players |> List.length |> (*) handSize)


        { Players = playerStates
          Active = List.head players
          Board = Empty
          Tiles = tiles }


    type Impl() =
        interface Rules<State> with
            member _.start players =
                players |> checkPlayerCount |> Result.map init
