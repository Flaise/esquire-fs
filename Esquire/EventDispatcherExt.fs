namespace Esquire

[<AutoOpen>]
module EventDispatcher =
    type EventDispatcher<'a> with
        member this.When (reactant:Reactant<bool>) =
            let result = new EventDispatcher<'a>()
            this.Listen (fun t -> if reactant.Value then result.Trigger t) |> ignore
            result
