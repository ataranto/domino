namespace Domino

type Player = {
    Id: int
    Name: string
}

type PlayerState = {
    Player: Player
    Tiles: Tile list
    Score: int
}

type State = {
    Players: PlayerState list
    Player: Player option
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
    let zero = {
        Players = List.empty
        Player = None
        Winner = None
        Board = Empty
        Boneyard = List.empty
    }

    let start names =
        let players =
            Seq.initInfinite id
            |> Seq.zip names
            |> Seq.map (fun (name, id) -> {
                Player = {
                    Id = id
                    Name = name
                }
                Tiles = List.empty
                Score = 0
            })

        { zero with Players = players |> Seq.toList }
