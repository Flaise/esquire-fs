namespace Esquire

type EventDispatcher<'a>() =
    let mutable listeners = []

    member this.Trigger (event:'a) =
        for listener in listeners do
            listener event
    
    member this.Listen (callback:'a->unit) =
        listeners <- callback::listeners
        // TODO: return a means by which to unbind the listener
    
    static member (+++) (r:EventDispatcher<'a>, s:EventDispatcher<'a>) =
        let result = new EventDispatcher<'a>()
        r.Listen <| fun t -> result.Trigger t
        s.Listen <| fun t -> result.Trigger t
        // TODO: unbind these listeners when result has no listeners
        result
