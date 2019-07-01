module Domino.Tests.Board

open Xunit
open FsUnit.Xunit

open Domino
open Board

module lead =
    [<Fact>]
    let ``when the tile is a double, it becomes the spinner`` () =
        let tile = Tile (0, 0)

        tile
        |> lead
        |> should equal (Node (tile, [
            0, Empty
            0, Empty
            0, Empty
            0, Empty
        ]))

    [<Fact>]
    let ``when the tile is not a double, it does not becomes the spinner`` () =
        let tile = Tile (0, 1)

        tile
        |> lead
        |> should equal (Node (tile, [
            0, Empty
            1, Empty
        ]))

module attach =
    [<Fact>]
    let ``when the target does not exist in the tree, the tree is not modified`` () =
        let tile = Tile (0, 1)

        tile
        |> lead
        |> attach (Tile (0, 6)) (Tile (6, 6))
        |> should equal (Node (tile, [
            0, Empty
            1, Empty
        ]))

    [<Fact>]
    let ``when the tile can not be attached to the target, the tree is not modified`` () =
        let target = Tile (0, 1)
        let tile = Tile (6, 6)

        target
        |> lead
        |> attach tile target
        |> should equal (Node (target, [
            0, Empty
            1, Empty
        ]))

    [<Fact>]
    let ``when the first double is attached, it becomes the spinner`` () =
        let tile = Tile (0, 0)
        let target = Tile (0, 1)

        target
        |> lead
        |> attach tile target
        |> should equal (Node (target, [
            0, Node (tile, [
                0, Empty
                0, Empty
                0, Empty
            ])
            1, Empty
        ]))

    [<Fact>]
    let ``each value of a node can only have one child attached`` () =
        let target = Tile (0, 1)
        let tile = Tile (0, 2)

        target
        |> lead
        |> attach tile target
        |> attach (Tile (0, 3)) target
        |> should equal (Node (target, [
            0, Node (tile, [2, Empty])
            1, Empty
        ]))

    [<Fact>]
    let ``no more than 2 tiles can be attached to a node`` () =
        let target = Tile (0, 1)

        target
        |> lead
        |> attach (Tile (0, 2)) target
        |> attach (Tile (1, 2)) target
        |> attach (Tile (1, 3)) target
        |> should equal  (Node (target, [
            1, Node (Tile (1, 2), [2, Empty])
            0, Node (Tile (0, 2), [2, Empty])
        ]))

    [<Fact>]
    let ``no more than 4 tiles can be attached to the spinner`` () =
        let target = Tile (0, 0)

        target
        |> lead
        |> attach (Tile (0, 1)) target
        |> attach (Tile (0, 2)) target
        |> attach (Tile (0, 3)) target
        |> attach (Tile (0, 4)) target
        |> attach (Tile (0, 5)) target
        |> should equal (Node (target, [
            0, Node (Tile (0,4), [4, Empty])
            0, Node (Tile (0,3), [3, Empty])
            0, Node (Tile (0,2), [2, Empty])
            0, Node (Tile (0,1), [1, Empty])
        ]))
