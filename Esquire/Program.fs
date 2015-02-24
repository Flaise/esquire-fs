open System


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
        r.Listen(fun t -> result.Trigger t)
        s.Listen(fun t -> result.Trigger t)
        // TODO: unbind these listeners when result has no listeners
        result


type Reactant<'a when 'a:equality>(initialFunc:unit->'a) =
    inherit EventDispatcher<'a*'a>()
    
    let mutable getValue = initialFunc
    let mutable lastValue = getValue ()
    
    new(initialValue:'a) = Reactant<'a>(fun () -> initialValue)

    member this.Updated () =
        let prev = lastValue
        let curr = this.Value
        
        if prev <> curr then
            lastValue <- curr // must be saved here because it might change during event propagation

            this.Trigger(prev, curr)

    member this.Value
        with get () = getValue ()
        and set (value) =
            getValue <- (fun () -> value)
            // TODO: unbind listeners if this reactant was previously derived from one or more other reactants
            this.Updated()

    member this.On (value:'a) =
        let result = new EventDispatcher<unit>()
        this.Listen (fun (prev, curr) -> if curr = value then result.Trigger ())
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

        
type EventDispatcher<'a> with
    member this.When (reactant:Reactant<bool>) =
        let result = new EventDispatcher<'a>()
        this.Listen(fun t -> if reactant.Value then result.Trigger t)
        result


let left = Reactant 4
let width = Reactant 3
let right = left + width

right.Listen (fun (prev, curr) -> Console.WriteLine("Changed right from " + string prev + " to " + string curr + "."))

let printStatus () =
    Console.WriteLine()
    Console.WriteLine("left: " + string left.Value)
    Console.WriteLine("width: " + string width.Value)
    Console.WriteLine("right: " + string right.Value)
    Console.WriteLine()

printStatus()

Console.WriteLine "Changing left..."
left.Value <- -2
Console.WriteLine "Changed left."

printStatus()


let keyLeft = Reactant false
let keyShift = Reactant false
let alive = Reactant true
let onAttackWest = keyLeft.On(true).When(keyShift &&& alive)

keyLeft.Listen(fun(prev, curr) -> if curr then Console.WriteLine "Pressed left arrow key" else Console.WriteLine "Released left arrow key")
keyShift.Listen(fun(prev, curr) -> if curr then Console.WriteLine "Pressed shift" else Console.WriteLine "Released shift")
onAttackWest.Listen(fun() -> Console.WriteLine "Attacked west")

keyLeft.Value <- true
keyShift.Value <- true

Console.WriteLine "Reassigning same value to keyLeft; this should not trigger listeners:"
keyLeft.Value <- true
Console.WriteLine "Done."

keyLeft.Value <- false
keyLeft.Value <- true


Console.WriteLine("Press Enter to continue...")
Console.ReadLine() |> ignore
