namespace Domino

type Rules<'State> =
    abstract member start: Player list -> Result<'State, string>

type SimpleState = { Foo: int }

type SimpleRules() =
    interface Rules<SimpleState> with
        member _.start players =
            if List.length players >= 2 && List.length players <= 4 then
                Ok { Foo = 3 }
            else
                Error "Number of players must be between 2 and 4"
