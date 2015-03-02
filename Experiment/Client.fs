namespace Experiment

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.Html.Client


type Vector2i = {X:int; Y:int}


type State = Map<int, obj>


[<AutoOpen; JavaScript>]
module States =
    let Get<'a> key getDefault (state:State) =
        match state.TryFind key with
        | None -> getDefault ()
        | Some result -> result :?> 'a

    let private getEmptySet () = Set.empty

    let GetSet<'a> key (state:State) =
        Get key getEmptySet state

    let private getEmptyMap () = Map.empty

    let GetMap<'a> key (state:State) =
        Get key getEmptyMap state

    let EmptyState = Map.empty
    

[<JavaScript>]
module Effects =
    type TypeID = int
    type Handler<'a> = 'a -> State -> 'a*State
    type Group<'a> = Map<int, Handler<'a>>
    type MetaGroup = Map<int, obj>

    let Key = 3

    let GetGroups (state:State) : MetaGroup =
        GetMap Key state

    let GetGroup typeID (state:State) : Group<'a> =
        match GetGroups(state).TryFind typeID with
        | None -> Map.empty
        | Some handlers -> handlers :?> Group<'a>

    let SetGroups (groups:MetaGroup) (state:State) =
        state.Add(Key, groups)

    let SetGroup typeID (group:Group<'a>) (state:State) =
        let groups = GetGroups state
        SetGroups (groups.Add(typeID, group)) state

    let Trigger typeID effect state =
        Map.fold
            (fun (effect,state) key handler -> handler effect state)
            (effect, state)
            (GetGroup typeID state)

    let Register typeID handler priority state =
            let group = GetGroup typeID state
            let inline update () = SetGroup typeID (group.Add(priority, handler)) state

        #if DEBUG
            match group.TryFind priority with
            | Some prevHandler -> failwith "Handler already specified for that type and priority."
            | None -> update()
        #else
            update()
        #endif

    let Unregister<'a> typeID priority state =
            let group:Group<'a> = GetGroup typeID state
            let inline update () = SetGroup typeID (group.Remove priority) state

        #if DEBUG
            match group.TryFind priority with
            | None -> failwith "No handler found for that type and priority."
            | Some _ -> update()
        #else
            update()
        #endif
            

[<JavaScript>]
type GentlePushEffect =
    {Origin: Vector2i
     Destination: Vector2i
     Obstructed: bool}
with
    static member TypeID = 1
    static member Default = {Origin={X=0; Y=0}
                             Destination={X=0; Y=0}
                             Obstructed=false}

[<JavaScript>]
type TickEffect() =
    class end
with
    static member TypeID = 2
    static member Default = TickEffect ()


[<JavaScript>]
module Floors =
    let Key = 2

    let GetPositions (state:State) =
        GetSet Key state

    let Present position state =
        GetPositions(state).Contains position

    let private handleGentlePush (effect:GentlePushEffect) (state:State) =
        if Present effect.Destination state then
            {effect with Obstructed=true}, state
        else
            effect, state

    let SetPositions (positions:Set<Vector2i>) (state:State) =
        match state.TryFind(Key), positions with
        | None, positions when positions.IsEmpty ->
            state
        | None, positions ->
            let state = Effects.Register GentlePushEffect.TypeID handleGentlePush 0 state
            state.Add(Key, positions)
        | Some _, positions when positions.IsEmpty ->
            let state = Effects.Unregister GentlePushEffect.TypeID 0 state
            state.Remove(Key)
        | Some _, positions ->
            state.Add(Key, positions)

    let Make (position:Vector2i) (state:State) =
        SetPositions (GetPositions(state).Add(position)) state


[<JavaScript>]
module Water =
    let Key = 1

    let GetPositions (state:State) =
        GetSet Key state

    let rec SetPositions (positions:Set<Vector2i>) (state:State) =
        match state.TryFind(Key), positions with
        | None, positions when positions.IsEmpty ->
            state
        | None, positions ->
            let state = Effects.Register TickEffect.TypeID handleTick 0 state
            state.Add(Key, positions)
        | Some _, positions when positions.IsEmpty ->
            let state = Effects.Unregister TickEffect.TypeID 0 state
            state.Remove(Key)
        | Some _, positions ->
            state.Add(Key, positions)
        
    and private fall (state:State) position =
        let dest = {position with Y = position.Y + 1}
        let positions = GetPositions(state)
        let effect = {GentlePushEffect.Default with Origin = position
                                                    Destination = dest}
        let effect, state = Effects.Trigger GentlePushEffect.TypeID effect state
        if effect.Obstructed then
            state
        else
            SetPositions (positions.Add dest) state

    and private handleTick (effect:TickEffect) (state:State) =
        let state = GetPositions(state)
                    |> Set.fold (fall) (SetPositions Set.empty state)
        effect, state

    let Make (position:Vector2i) (state:State) =
        SetPositions (GetPositions(state).Add(position)) state


[<JavaScript>]
module Client =
    // Since IE does not support canvas natively, initialization of the 
    // canvas element is done through the excanvas.js library.
    [<Inline "G_vmlCanvasManager.initElement($elem)">]
    let Initialize (elem: CanvasElement) : unit = ()

    [<Inline "requestAnimationFrame($0)">]
    let render (frame: unit->unit) = X<unit>

    let draw (context: CanvasRenderingContext2D) (state:State) =
        let canvas = As<CanvasElement> context.Canvas
        context.ClearRect(0., 0., float canvas.Width, float canvas.Height)
        context.StrokeRect(0., 0., float canvas.Width, float canvas.Height)
        
        let size = 5.

        context.FillStyle <- "blue"
        for position in Water.GetPositions(state) do
            context.FillRect(size * float position.X, size * float position.Y, size, size)

        context.FillStyle <- "brown"
        for position in Floors.GetPositions(state) do
            context.FillRect(size * float position.X, size * float position.Y, size, size)
        
    let Main () =
        let state = EmptyState
                    |> Water.Make {X=1; Y=1}
                    |> Water.Make {X=3; Y=2}
                    |> Water.Make {X=3; Y=1}
                    |> Water.Make {X=3; Y=3}
                    |> Floors.Make {X=1; Y=15}
                    |> Floors.Make {X=2; Y=16}
                    |> Floors.Make {X=3; Y=16}
                    |> ref
            
        let element = Canvas [Attr.Style "margin: 0 auto;"]
        let canvas  = As<CanvasElement> element.Dom
        // Conditional initialization for the case of IE.
        if (canvas |> JS.Get "getContext" = JS.Undefined) then
            Initialize canvas
        canvas.Height <- 200
        canvas.Width  <- 200

        let update () =
            let effect = TickEffect.Default
            let effect, newState = Effects.Trigger TickEffect.TypeID effect !state
            state := newState

        JS.SetInterval update 300 |> ignore

        let rec frame () =
            render frame
            draw (canvas.GetContext "2d") !state
        render frame

        element


type CanvasViewer() =
    inherit Web.Control()
    [<JavaScript>]
    override this.Body = Client.Main () :> _

