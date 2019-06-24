module Domino.Tests.Tile

open Xunit
open FsUnit.Xunit

open Domino

[<Fact>]
let ``foo`` () =
    let tile = Tile (0, 0)
    tile |> Tile.values |> should equal [0; 0]
