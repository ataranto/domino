module Domino.Tests.Events

open Xunit
open FsUnit.Xunit

open Domino
open Events

module zero =
    [<Fact>]
    let ``the zero state has an empty list`` () =
        zero
        |> fun state -> state.Players
        |> should be FsUnit.CustomMatchers.Empty

    [<Fact>]
    let ``the zero state Player optional value is None`` () =
        zero
        |> fun state -> state.Player
        |> should equal None

    [<Fact>]
    let ``the zero state Winner optional value is None`` () =
        zero
        |> fun state -> state.Winner
        |> should equal None

    [<Fact>]
    let ``the zero state Board value is Empty`` () =
        zero
        |> fun state -> state.Board
        |> should equal Empty


    [<Fact>]
    let ``the zero state Bonyard value is an empty list`` () =
        zero
        |> fun state -> state.Boneyard
        |> should be FsUnit.CustomMatchers.Empty
