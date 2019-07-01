module Domino.Tests.Board

open Xunit
open FsUnit.Xunit

open Domino
open Board

module lead =
    [<Fact>]
    let ``when the tile is a double, it becomes the spinner`` () =
        let tile = Tile (0, 0)
        tile |> lead |> should equal (Spinner (tile, []))

    [<Fact>]
    let ``when the tile is not a double, it does not becomes the spinner`` () =
        let tile = Tile (0, 1)
        tile |> lead |> should equal (Node (tile, []))
