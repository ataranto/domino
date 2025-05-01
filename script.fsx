#load "src/Domino/Types.fs"

open Domino

let leaves tree =
    let rec loop acc =
        function
        | Empty -> acc
        | Node(tile, children) ->
            children
            |> List.collect (function
                | value, Empty -> Leaf(tile, value) :: acc
                | _, tree -> tree |> loop acc)

    tree |> loop List.empty

let lead tile =
    Node(tile, tile |> Tile.values |> List.map (fun value -> value, Empty))

let attach tile target tree =
    let rec loop =
        function
        | Empty -> Empty
        | Node(t, children) when t = target ->
            Node(
                t,
                children
                |> List.map (function
                    | value, children when tile |> Tile.values |> List.contains value ->
                        value,
                        Node(
                            tile,
                            tile
                            |> Tile.values
                            |> List.choose (fun v -> if v <> value then Some(v, Empty) else None)
                        )
                    | x -> x)
            )
        | Node(t, children) -> Node(t, children |> List.map (fun (v, tree) -> v, tree |> loop))

    tree |> loop

let tree =
    Tile(1, 2)
    |> lead
    |> attach (Tile(2, 3)) (Tile(1, 2))
    |> attach (Tile(3, 4)) (Tile(2, 3))


// Example usage:
tree |> leaves
