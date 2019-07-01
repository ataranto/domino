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

module actions =
    [<Fact>]
    let ``when the board is empty, any tile can be played to lead`` () =
        let tile = Tile (0, 0)
        let tiles = tile |> List.singleton

        Empty
        |> actions tiles
        |> should equal [Lead tile]

    [<Fact>]
    let ``when a tile has an empty edge, a matching tile can be attached`` () =
        let target = Tile (0, 0)
        let tile = Tile (0, 1)
        let tiles = tile |> List.singleton

        target
        |> lead
        |> actions tiles
        |> should equal [Attach (tile, target)]

    [<Fact>]
    let ``when none of the tiles may be played, an empty list is returned`` () =
        let target = Tile (0, 0)
        let tile = Tile (1, 1)
        let tiles = tile |> List.singleton

        target
        |> lead
        |> actions tiles
        |> should be FsUnit.CustomMatchers.Empty

module score =
    [<Fact>]
    let ``when no tiles have been played, the score is 0`` () =
        Empty |> score |> should equal 0

    [<Fact>]
    let ``when the first tile played is a double 5, the score is 10`` () =
        Tile (5, 5) |> lead |> score |> should equal 10

    [<Fact>]
    let ``when the spinner is played first, it is counted correctly`` () =
        let tile = Tile (6, 6)

        [
            attach (Tile (3, 6)) tile
            attach (Tile (2, 6)) tile
            attach (Tile (5, 6)) tile
        ]
        |> List.scan (fun tree attach -> attach tree) (tile |> lead)
        |> List.map score
        |> should equal [0; 15; 5; 10]

    [<Fact>]
    let ``when the spinner is not played first, it is counted correctly`` () =
        let tile = Tile (3, 6)

        [
            attach (Tile (6, 6)) tile
            attach (Tile (2, 6)) (Tile (6, 6))
            attach (Tile (5, 6)) (Tile (6, 6))
        ]
        |> List.scan (fun tree attach -> attach tree) (tile |> lead)
        |> List.map score
        |> should equal [0; 15; 5; 10]

    [<Fact>]
    let ``sequence 1 is scored correctly`` () =
        let tile = Tile (3, 6)

        [
            attach (Tile (6, 6)) tile
            attach (Tile (3, 4)) tile
            attach (Tile (4, 4)) (Tile (3, 4))
        ]
        |> List.scan (fun board attach -> attach board) (tile |> lead)
        |> List.map score
        |> should equal [0; 15; 0; 20]

    [<Fact>]
    let ``sequence 2 is scored correctly`` () =
        let tile = Tile (1, 4)

        [
            attach (Tile (1, 1)) tile
            attach (Tile (4, 4)) tile
            attach (Tile (3, 4)) (Tile (4, 4))
            attach (Tile (1, 2)) (Tile (1, 1))
            attach (Tile (1, 5)) (Tile (1, 1))
            attach (Tile (5, 5)) (Tile (1, 5))
            attach (Tile (0, 5)) (Tile (5, 5))
            attach (Tile (1, 3)) (Tile (1, 1))
        ]
        |> List.scan (fun board attach -> attach board) (tile |> lead)
        |> List.map score
        |> should equal [5; 0; 10; 5; 5; 10; 15; 5; 0]
