module Domino.Tests.Rules

open Xunit
open FsUnit.Xunit
open Domino

type ``SimpleRules Tests``() =
    let rules = SimpleRules() :> Rules

    [<Fact>]
    let ``SimpleRules.start should return Error for invalid player count`` () =
        [ { Id = 0; Name = "Player 1" } ]
        |> rules.start
        |> Result.isError
        |> should be True
