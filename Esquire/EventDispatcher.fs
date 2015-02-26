namespace Esquire

open System.Runtime.CompilerServices


type EventDispatcher<'a>() =
    let mutable listeners = []

    member this.Trigger (event:'a) =
        for listener in listeners do
            listener event
    
    member this.Listen (callback:'a->unit) =
        listeners <- callback::listeners
        // TODO: return a means by which to unbind the listener
        fun () -> ()
    
    static member (+++) (r:EventDispatcher<'a>, s:EventDispatcher<'a>) =
        let result = EventDispatcher<'a> ()
        (r.Listen <| fun t -> result.Trigger t) |> ignore
        (s.Listen <| fun t -> result.Trigger t) |> ignore
        // TODO: unbind these listeners when result has no listeners
        result


[<Extension>]
type Registration() =
    [<Extension>]
    static member inline Remove(registration: unit -> unit) = registration ()
