#load "src/Domino/Types.fs"
#load "src/Domino/Rules.fs"

open Domino
open Domino.SimpleRules


let rules = Domino.SimpleRules.Impl() :> Rules<SimpleRules.State>
let players = [ { Id = 0; Name = "Alice" }; { Id = 1; Name = "Bob" } ]

let state = players |> rules.start
printfn "%A" state
