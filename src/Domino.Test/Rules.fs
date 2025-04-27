module Domino.Tests.Rules

open Xunit
open FsUnit.Xunit
open FsUnitTyped
open Domino
open Domino.SimpleRules

module SimpleRules =

    let rules = SimpleRules.Impl() :> Rules<SimpleRules.State>

    [<Fact>]
    let ``start should return Error for invalid player count`` () =
        [ { Id = 0; Name = "Player 1" } ]
        |> rules.start
        |> Result.isError
        |> should be True

    type ``start initial state``() =
        let players = [ { Id = 0; Name = "Player 1" }; { Id = 1; Name = "Player 2" } ]
        let result = players |> rules.start

        let state =
            match result with
            | Ok state -> state
            | Error err -> failwithf "Expected Ok but got Error: %s" err

        [<Fact>]
        let ``should be Ok`` () = state

        [<Fact>]
        let ``should start with an Empty board`` () = state.Board |> shouldEqual Empty

        [<Fact>]
        let ``should give each player 7 tiles`` () =
            state.Players
            |> Map.forall (fun _ playerState -> playerState.Tiles.Length = 7)
            |> should be True

        [<Fact>]
        let ``should have 14 remaining tiles`` () =
            state.Tiles |> List.length |> should equal 14
