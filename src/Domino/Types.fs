namespace Domino

type Tile = Tile of int * int

type Player = { Id: int; Name: string }

type PlayerState = { Tiles: Tile list; Score: int }

type Tree<'Node> =
    | Empty
    | Node of 'Node * Tree<'Node> list
