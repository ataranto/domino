module Domino.Tests.Rules

open Xunit
open FsUnit.Xunit
open FsUnitTyped
open Domino

module SimpleRules =
    open Domino.SimpleRules
    let rules = SimpleRules.Impl() :> Rules<SimpleRules.State>

    [<Fact>]
    let ``tile set should contain 28 tiles`` () = tiles |> Set.count |> shouldEqual 28

    type ``tile weight``() =
        [<Fact>]
        let ``should be unique for each tile`` () =
            tiles
            |> Set.toList
            |> List.distinctBy weight
            |> List.length
            |> shouldEqual tiles.Count

        [<Fact>]
        let ``should rank higher doubles higher`` () =
            let tile1 = Tile(6, 6) |> weight
            let tile2 = Tile(5, 5) |> weight

            tile1 |> shouldBeGreaterThan tile2

        [<Fact>]
        let ``should rank doubles higher than non doubles`` () =
            let weight1 = Tile(0, 0) |> weight
            let weight2 = Tile(5, 6) |> weight

            weight1 |> shouldBeGreaterThan weight2

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
        let ``should add each player`` () =
            state.Players |> Map.count |> shouldEqual players.Length

        [<Fact>]
        let ``should give each player 7 tiles`` () =
            state.Players
            |> Map.forall (fun _ playerState -> playerState.Tiles.Length = 7)
            |> should be True

        [<Fact>]
        let ``should start with an Empty board`` () = state.Board |> shouldEqual Empty

        [<Fact>]
        let ``should set the Active player based on the tile with the highest weight`` () =
            let maxWeight =
                state.Players
                |> Map.find state.Active
                |> fun ps -> ps.Tiles |> List.map weight |> List.max

            state.Players
            |> Map.toList
            |> List.collect (fun (_, ps) -> ps.Tiles)
            |> List.forall (fun tile -> tile |> weight <= maxWeight)
            |> should be True

        [<Fact>]
        let ``should have 14 remaining tiles`` () =
            state.Tiles |> List.length |> should equal 14
