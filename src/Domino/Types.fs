namespace Domino

module List =
    let rec remove value =
        function
        | [] -> []
        | x :: xs when x = value -> xs
        | x :: xs -> x :: remove value xs

type Tile = Tile of int * int

module Tile =
    let values (Tile(x, y)) = [ x; y ]

type Tree =
    | Empty
    | Node of Tile * Edge list

and Edge = int * Tree

module Tree =
    let empty values =
        values |> List.map (fun value -> value, Empty)



type Player = { Id: int; Name: string }

type PlayerState = { Tiles: Tile list; Score: int }
