module Domino.Tests.Game

open Xunit
open FsUnit.Xunit

open Domino

[<Fact>]
let ``Game.start should initialize the game state correctly`` () =
    let rules = OldSimpleRules.Impl()

    [ { Id = 0; Name = "Alice" }; { Id = 1; Name = "Bob" } ]
    |> Game.start rules
    |> Result.isOk
    |> should be True


[<Fact>]
let ``Game.start should fail if the players list is empty`` () =
    let rules = OldSimpleRules.Impl()
    [] |> Game.start rules |> Result.isError |> should be True
