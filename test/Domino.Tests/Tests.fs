module Domino.Tests

open Xunit
open FsUnit.Xunit

open Domino

[<Fact>]
let ``My test`` () =
    "Test" |> hello |> should equal "Hello Test"
