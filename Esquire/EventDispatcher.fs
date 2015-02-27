namespace Esquire

open System.Runtime.CompilerServices
open System.Collections.Generic


type EventDispatcher<'a>() =
    let listeners = LinkedList<'a->unit> ()

    member this.Trigger (event:'a) =
        for listener in listeners do
            listener event
    
    member this.Listen (callback:'a->unit) =
        let node = LinkedListNode callback
        listeners.AddFirst node
        fun () -> listeners.Remove node
    
    static member (+++) (r:EventDispatcher<'a>, s:EventDispatcher<'a>) =
        let result = EventDispatcher<'a> ()
        r.Listen (fun t -> result.Trigger t) |> ignore
        s.Listen (fun t -> result.Trigger t) |> ignore
        // TODO: unbind these listeners when result has no listeners
        result


[<Extension>]
type Registration() =
    [<Extension>]
    static member inline Remove(registration: unit -> unit) = registration ()
