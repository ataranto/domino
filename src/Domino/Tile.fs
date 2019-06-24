namespace Domino

type Tile = Tile of int * int

module Tile =
    let values = function
        Tile (x, y) -> [x; y]
