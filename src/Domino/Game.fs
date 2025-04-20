namespace Domino


module Game =
    type State = { Foo: int }

    /// Creates a new game with the given players.
    /// Returns the initial sequence of events.
    let start (rules: Rules) players =
        let result = rules.start players

        match result with
        | Ok _ -> Ok { Foo = 3 }
        | Error error -> Error result
