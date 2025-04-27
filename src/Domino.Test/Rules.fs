module Domino.Tests.Rules

open Xunit
open FsUnit.Xunit
open FsUnitTyped
open Domino
open Domino.SimpleRules

type ``SimpleRules Tests``() =

    let rules = SimpleRules.Impl() :> Rules<SimpleRules.State>

    [<Fact>]
    let ``SimpleRules.start should return Error for invalid player count`` () =
        [ { Id = 0; Name = "Player 1" } ]
        |> rules.start
        |> Result.isError
        |> should be True

    [<Fact>]
    let ``SimpleRules.start should return Ok for valid player count`` () =
        let players = [ { Id = 0; Name = "Player 1" }; { Id = 1; Name = "Player 2" } ]
        let result = rules.start players

        let state =
            match result with
            | Ok state -> state
            | Error err -> failwithf "Expected Ok but got Error: %s" err

        state.Players |> Map.count |> should equal 2
        state.Tiles |> List.length |> should equal 14
