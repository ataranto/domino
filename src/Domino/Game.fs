namespace Domino

open Events

module Game =
    /// Creates a new game with the given players.
    /// Returns the initial sequence of events.
    let newGame players =
        Events.start players

    ()
