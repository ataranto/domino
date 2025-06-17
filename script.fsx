#load "src/Domino/Types.fs"
#load "src/Domino/Rules.fs"

open Domino
open Domino.EventSimpleRules

let state = Waiting { Players = Set.empty }

let actions =
    [ AddPlayer { Id = 1; Name = "Alice" }
      AddPlayer { Id = 2; Name = "Bob" }
      StartGame ]

let result =
    (state, actions)
    ||> List.scan (fun state action ->
        let events = state |> decide action

        match events with
        | Error error -> state
        | Ok events -> (state, events) ||> List.fold evolve)
