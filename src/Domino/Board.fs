namespace Domino

type Tree =
    | Empty
    | Node of Tile * (int * Tree) list

type Action =
    | Lead of Tile
    | Attach of Tile * Tile

module Board =
    // http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list
    let rec private remove x = function
        | x'::xs when x' = x -> xs
        | x'::xs             -> x'::remove x xs
        | []                 -> []

    let private empty values =
        values |> List.map (fun value -> value, Empty)

    let lead = function
        | Tile (x, y) as tile when x = y ->
            Node (tile, x |> List.replicate 4 |> empty)
        | tile ->
            Node (tile, tile |> Tile.values |> empty)

    let rec private spinner = function
        | Empty ->
            None
        | Node (tile, edges) when edges.Length > 2 ->
            Some tile
        | Node (_, edges) ->
            edges |> List.map snd |> List.tryPick spinner

    let rec private intersect = function
        | _, [] ->
            None
        | Tile (x, y), (value, Empty)::_ when value = x || value = y ->
            Some value
        | tile, _::edges ->
            (tile, edges) |> intersect

    let attach tile target tree =
        let create value =
            match tile, tree |> spinner with
            | Tile (x, y), None when x = y ->
                Node (tile, x |> List.replicate 3 |> empty)
            | _ ->
                Node (tile, tile |> Tile.values |> remove value |> empty)
        let rec loop = function
            | Empty ->
                Empty
            | Node (t, edges) when t = target ->
                match (tile, edges) |> intersect with
                | None -> Node (t, edges)
                | Some value ->
                    let edge = value, value |> create
                    Node (t, edge::(edges |> remove (value, Empty)))
            | Node (t, edges) ->
                Node (t, edges |> List.map (fun (v, tree) -> v, tree |> loop))
        tree |> loop

    let private edges tree =
        let rec loop result = function
            | Empty -> result
            | Node (tile, edges') ->
                edges'
                |> List.collect (function
                    | value, Empty -> (tile, value)::result
                    | _, tree      -> tree |> loop result
                )
        tree |> loop List.empty

    let actions tiles = function
        | Empty ->
            tiles |> List.map Lead
        | node ->
            node
            |> edges
            |> List.allPairs tiles
            |> List.choose (function
                | Tile (x, y), (tile, value) when x = value || y = value ->
                    Some (Attach (Tile (x, y), tile))
                | _ ->
                    None
            )
            |> List.distinct
