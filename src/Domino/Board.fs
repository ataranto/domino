namespace Domino

type Tree =
    | Empty
    | Node of Tile * Tree list
    | Spinner of Tile * Tree list

module Board =
    let lead = function
        | Tile (x, y) as tile when x = y ->
            Spinner (tile, [])
        | tile ->
            Node (tile, [])
