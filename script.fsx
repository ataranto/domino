#load "src/Domino/Types.fs"

open Domino

let tree =
    let tile1 = Tile(1, 2)
    let tile2 = Tile(2, 3)
    let tile3 = Tile(3, 4)
    Node(tile1, [ Node(tile2, [ Node(tile3, []) ]) ])

let leaves tree =
    let rec loop acc =
        function
        | Empty -> acc
        | Node(tile, []) -> tile :: acc
        | Node(tile, children) -> children |> List.fold (fun a c -> loop a c) acc

    match tree with
    | Node(tile, [ _ ]) -> tree |> loop [ tile ]
    | tree -> tree |> loop List.empty

// Example usage:
tree |> leaves
