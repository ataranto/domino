#load "Tile.fs"
#load "Board.fs"

open Domino

let tree =
    Tile (0, 1)
    |> Board.lead
    |> Board.attach (Tile (0, 2)) (Tile (0, 1))
    |> Board.attach (Tile (1, 6)) (Tile (0, 1))

    |> Board.actions [Tile (6, 6)]
