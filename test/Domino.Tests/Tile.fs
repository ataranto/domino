module Domino.Tests.Tile

open Xunit
open FsUnit.Xunit

open Domino

[<Fact>]
let ``values returns the tile values as a list`` () =
    Tile (0, 0) |> Tile.values |> should equal [0; 0]
