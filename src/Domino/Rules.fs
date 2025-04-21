namespace Domino

type Rules =
    abstract member start: Player list -> Result<unit, string>

type SimpleRules() =
    interface Rules with
        member _.start players =
            if List.length players >= 2 && List.length players <= 4 then
                Ok()
            else
                Error "Number of players must be between 2 and 4"
