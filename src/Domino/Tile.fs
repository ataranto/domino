namespace Domino

type Tile = Tile of int * int

module Tile =
    let values = function
        Tile (x, y) -> [x; y]

    let tiles n =
        [ for x in 0..n do for y in x..n do yield x, y ]
        |> List.map Tile
