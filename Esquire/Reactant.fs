namespace Esquire


type Reactant<'a when 'a:equality> (initialFunc:unit->'a) =
    inherit EventDispatcher<'a*'a>()
    
    let mutable getValue = initialFunc
    let mutable lastValue = getValue ()
    
    new(initialValue:'a) = Reactant<'a> (fun () -> initialValue)

    member this.Derive (func, dispatchers:EventDispatcher<'b> array) =
        let update (_:'b) = this.Updated ()

        for dispatcher in dispatchers do
            dispatcher.Listen (update) |> ignore

        this.Derive func

    member this.Derive (func, dispatcher:EventDispatcher<'b>) =
        dispatcher.Listen (fun _ -> this.Updated ()) |> ignore
        this.Derive func

    member this.Derive func =
        getValue <- func
        this.Updated ()

    static member Derivation (func:unit->'a) (dispatcher:EventDispatcher<_>): Reactant<'a> =
        let result = Reactant<'a> func
        dispatcher.Listen (fun _ -> result.Updated ()) |> ignore
        result

    member this.Updated () =
        let prev = lastValue
        let curr = this.Value
        
        if prev <> curr then
            lastValue <- curr // must be saved here because it might change during event propagation

            this.Trigger(prev, curr)

    member this.Value
        with get () = getValue ()
        and set (value) = this.Derive <| fun () -> value

    member this.On (value:'a) =
        let result = EventDispatcher<unit> ()
        this.Listen (fun (prev, curr) -> if curr = value then result.Trigger ()) |> ignore
        result

    member this.OnChange () =
        let result = EventDispatcher<unit> ()
        this.Listen (fun (prev, curr) -> result.Trigger ()) |> ignore
        result

    member this.Compose (other:Reactant<'b>) (transformation:'a->'b->'c) =
        let func () = transformation this.Value other.Value
        Reactant.Derivation func (this.OnChange () +++ other.OnChange ())

    member this.Transform (transformation:'a->'c) =
        let func () = transformation this.Value
        Reactant.Derivation func this
        
    static member (+) (r:Reactant<int>, s:Reactant<int>) =
        r.Compose s (fun t u -> t + u)

    static member (+) (r:Reactant<float>, s:Reactant<float>) =
        r.Compose s (fun t u -> t + u)

    static member (&&&) (r:Reactant<bool>, s:Reactant<bool>) =
        r.Compose s (fun t u -> t && u)

    static member (|||) (r:Reactant<bool>, s:Reactant<bool>) =
        r.Compose s (fun t u -> t || u)
