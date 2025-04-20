module Domino.Tests.Game

open Xunit
open FsUnit.Xunit

open Domino
open Domino.Game

// Add tests for the Game module here
[<Fact>]
let ``placeholder test`` () = true |> should be True

[<Fact>]
let ``Game.start should initialize the game state correctly`` () =
    let rules = Domino.SimpleRules()

    let players = [ { Name = "Alice"; Id = 0 }; { Name = "Bob"; Id = 1 } ]
    let result = Game.start rules players

    match result with
    | Ok state -> state |> should equal { Foo = 3 }
    | Error err -> failwithf "Unexpected error: %A" err


[<Fact>]
let ``Game.start should fail if the players list is empty`` () =
    let rules = Domino.SimpleRules()
    let players = [] // Empty players list
    let result = Game.start rules players

    match result with
    | Error _ -> ()
    | _ -> failwith "Expected a Result.Error but got a Result.Ok"
