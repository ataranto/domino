namespace Domino


module Game =
    type State = { Foo: int }

    /// Creates a new game with the given players.
    /// Returns the initial sequence of events.
    let start (rules: Rules) players =
        players |> rules.start |> Result.map (fun _ -> { Foo = 3 })
