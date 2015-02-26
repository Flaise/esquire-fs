namespace Esquire

type Reactant<'a when 'a:equality>(initialFunc:unit->'a) =
    inherit EventDispatcher<'a*'a>()
    
    let mutable getValue = initialFunc
    let mutable lastValue = getValue ()
    
    new(initialValue:'a) = Reactant<'a>(fun () -> initialValue)

    member this.Derive (func, ?dispatcher) =
        getValue <- func
        // TODO: unbind listeners if this reactant was previously derived from one or more other reactants
        this.Updated ()

    member this.Updated () =
        let prev = lastValue
        let curr = this.Value
        
        if prev <> curr then
            lastValue <- curr // must be saved here because it might change during event propagation

            this.Trigger(prev, curr)

    member this.Value
        with get () = getValue ()
        and set (value) =
            this.Derive <| fun () -> value

    member this.On (value:'a) =
        let result = new EventDispatcher<unit>()
        this.Listen <| fun (prev, curr) -> if curr = value then result.Trigger ()
        result

    member this.Compose (other:Reactant<'a>) (transformation:'a->'a->'a) =
        let result = new Reactant<'a>(fun () -> transformation this.Value other.Value)
        (this +++ other).Listen(fun(prev, curr) -> result.Updated())
        result
        
    static member (+) (r:Reactant<int>, s:Reactant<int>) =
        r.Compose s (fun t u -> t + u)

    static member (+) (r:Reactant<float>, s:Reactant<float>) =
        r.Compose s (fun t u -> t + u)

    static member (&&&) (r:Reactant<bool>, s:Reactant<bool>) =
        r.Compose s (fun t u -> t && u)

    static member (|||) (r:Reactant<bool>, s:Reactant<bool>) =
        r.Compose s (fun t u -> t || u)
