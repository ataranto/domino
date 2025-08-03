module Domino.Tests.Rules

open Xunit
open FsUnit.Xunit
open FsUnitTyped
open Domino

let players =
    [ 0..3 ]
    |> List.map (fun i ->
        { Id = i
          Name = sprintf "Player %d" (i + 1) })

module SimpleRules =
    open Domino.OldSimpleRules
    let rules = Impl() :> Rules<State, Action>

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

    type ``start initial state``(output: ITestOutputHelper) =
        let playerCount = 2
        let result = players |> List.take playerCount |> rules.start

        let state =
            match result with
            | Ok state -> state
            | Error err -> failwithf "Expected Ok but got Error: %s" err

        [<Fact>]
        let ``should be Ok`` () = state

        [<Fact>]
        let ``should add each player`` () =
            state.Players |> Map.count |> shouldEqual playerCount

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

    type SequenceTest(output: ITestOutputHelper) =
        // initial non-random state, tiles sorted by weight
        let state =
            let players = players |> List.take 2

            let hands, boneyard =
                tiles |> Set.toList |> List.sortByDescending weight |> deal players

            let playerStates = hands |> Map.map (fun _ tiles -> { Tiles = tiles; Score = 0 })

            { Players = playerStates
              Active = players |> List.head
              Board = Empty
              Tiles = boneyard }

        let actions = [ Lead(Tile(6, 6)); Attach(Tile(5, 6), Tile(6, 6)) ]

        let states =
            actions
            |> List.scan (fun state action -> state |> rules.play action |> Result.defaultValue state) state

        [<Fact>]
        member _.``state 1: Lead(Tile(6, 6))``() =
            let state = states |> List.item 1

            state.Board
            |> shouldEqual (Node(Tile(6, 6), [ 6, Empty; 6, Empty; 6, Empty; 6, Empty ]))

            // check that tile was removed from player's hand
            state.Players
            |> Map.find players.[0]
            |> fun ps -> ps.Tiles |> List.exists (fun tile -> tile = Tile(6, 6)) |> should be False

            // check that next player is active
            state.Active |> shouldEqual (players |> List.item 1)


        [<Fact>]
        member _.``state 2: Attach(Tile(5, 6), Tile(6, 6))``() =

            let state = states |> List.item 2

            output.WriteLine "==="
            output.WriteLine $"%A{state}"
            output.WriteLine $"%A{state.Board}"
            output.WriteLine $"edges: %A{state.Board |> edges}"
            output.WriteLine $"actions: %A{state |> rules.actions}"

            state.Board
            |> shouldEqual (Node(Tile(6, 6), [ 6, Node(Tile(5, 6), [ 5, Empty ]); 6, Empty; 6, Empty; 6, Empty ]))
