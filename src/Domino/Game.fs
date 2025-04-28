namespace Domino


module Game =
    let start (rules: Rules<'State, 'Action>) players = players |> rules.start
