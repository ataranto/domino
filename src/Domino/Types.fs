namespace Domino

type Tile = Tile of int * int

type Tree =
    | Empty
    | Node of Tile * Tree list

type Player = { Id: int; Name: string }

type PlayerState = { Tiles: Tile list; Score: int }
