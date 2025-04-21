module Domino.Tests.Game

open Xunit
open FsUnit.Xunit

open Domino

// Add tests for the Game module here
[<Fact>]
let ``placeholder test`` () = true |> should be True

[<Fact>]
let ``Game.start should initialize the game state correctly`` () =
    let rules = Domino.SimpleRules()

    [ { Id = 0; Name = "Alice" }; { Id = 1; Name = "Bob" } ]
    |> Game.start rules
    |> Result.contains { Foo = 3 }
    |> should be True


[<Fact>]
let ``Game.start should fail if the players list is empty`` () =
    let rules = Domino.SimpleRules()
    [] |> Game.start rules |> Result.isError |> should be True
