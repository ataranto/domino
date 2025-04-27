namespace Domino


module Game =
    let start (rules: Rules<'State>) players = players |> rules.start
