namespace Domino

type Tile = Tile of int * int
type Leaf = Tile * int

module Tile =
    let values (Tile(x, y)) = [ x; y ]

type Tree =
    | Empty
    | Node of Tile * (int * Tree) list

type Player = { Id: int; Name: string }

type PlayerState = { Tiles: Tile list; Score: int }
