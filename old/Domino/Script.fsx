#load "Tile.fs"
#load "Board.fs"

open Domino

let score =
    Tile (4, 5)
    |> Board.lead
    |> Board.attach (Tile (5, 6)) (Tile (4, 5))
    |> Board.score
