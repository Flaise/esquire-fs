namespace Experiment

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.Html.Client


type State = Map<int, obj>


[<AutoOpen; JavaScript>]
module States =
    let EmptyState = Map.empty

    let Get key getDefault (state:State) : 'a =
        match state.TryFind key with
        | None -> getDefault ()
        | Some result -> result :?> 'a

    let private getEmptySet () = Set.empty
    let GetSet key (state:State) : Set<'a> =
        Get key getEmptySet state

    let private getEmptyMap () = Map.empty
    let GetMap key (state:State) : Map<'a, 'b> =
        Get key getEmptyMap state

    let private getEmptyList () = []
    let GetList key (state:State) : list<'a> =
        Get key getEmptyList state


type Vector2i = {X:int; Y:int}
    

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


[<JavaScript; AutoOpen>]
module EffectStates =
    let SetState key data register unregister (state:State) =
        match state.TryFind(key), data with
        | None, data when Seq.isEmpty data ->
            state
        | None, data ->
            let state:State = register state
            state.Add(key, data)
        | Some _, data when Seq.isEmpty data ->
            let state:State = unregister state
            state.Remove(key)
        | Some _, data ->
            state.Add(key, data)


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
module Bedrock =
    let Key = 5

    let GetSurfaces (state:State) =
        GetMap Key state

    let Present position state =
        match GetSurfaces(state).TryFind position.X with
        | None -> false
        | Some y -> y <= position.Y

    let private handleGentlePush (effect:GentlePushEffect) (state:State) =
        if Present effect.Destination state then
            {effect with Obstructed=true}, state
        else
            effect, state

    let SetSurfaces (positions:Map<int, int>) (state:State) =
        SetState
            Key
            positions
            (Effects.Register GentlePushEffect.TypeID handleGentlePush 1)
            (Effects.Unregister<GentlePushEffect> GentlePushEffect.TypeID 1)
            state

    let Make (position:Vector2i) (state:State) =
        SetSurfaces (GetSurfaces(state).Add(position.X, position.Y)) state

    let GetElevation x state =
        GetSurfaces(state).TryFind x


[<JavaScript>]
type TickEffect() =
    class end
with
    static member TypeID = 2
    static member Default = TickEffect()


[<JavaScript>]
module Floors =
    let Key = 2

    let GetPositions (state:State) =
        GetSet Key state

    let Present position state =
        GetPositions(state).Contains position

    let rec SetPositions (positions:Set<Vector2i>) (state:State) =
        SetState
            Key
            positions
            (fun state ->
                state
                |> Effects.Register TickEffect.TypeID handleTick -1
                |> Effects.Register GentlePushEffect.TypeID handleGentlePush 0
            )
            (fun state ->
                state
                |> Effects.Unregister<TickEffect> TickEffect.TypeID -1
                |> Effects.Unregister<GentlePushEffect> GentlePushEffect.TypeID 0
            )
            state
        
    and private fall position (prevState:State) (state:State) =
        let left = {position with X = position.X - 1}
        let right = {position with X = position.X + 1}
        let destination = {position with Y = position.Y + 1}
        let positions = GetPositions prevState
        let newPositions = GetPositions state
        if positions.Contains(left) || positions.Contains(right) || positions.Contains(destination)
                || Bedrock.Present destination prevState then
            SetPositions (newPositions.Add position) state
        else
            SetPositions (newPositions.Add destination) state

    and private handleTick (effect:TickEffect) (state:State) =
        let mutable newState = SetPositions Set.empty state
        for position in GetPositions state do
            newState <- fall position state newState
        effect, newState

    and private handleGentlePush (effect:GentlePushEffect) (state:State) =
        if Present effect.Destination state then
            let effect = {effect with Obstructed=true}
            let below = {effect.Destination with Y = effect.Destination.Y + 1}
            if Present below state || Bedrock.Present below state then
                effect, state
            else
                let state = SetPositions
                                (GetPositions(state).Remove(effect.Destination).Add(below))
                                state
                effect, state
        else
            effect, state

    let Make (position:Vector2i) (state:State) =
        SetPositions (GetPositions(state).Add(position)) state


[<JavaScript>]
module Water =
    let Key = 1

    let GetPositions (state:State) =
        GetSet Key state

    let rec SetPositions (positions:Set<Vector2i>) (state:State) =
        SetState
            Key
            positions
            (Effects.Register TickEffect.TypeID handleTick 0)
            (Effects.Unregister<TickEffect> TickEffect.TypeID 0)
            state
        
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


type RainEmitter = {
    Corner: Vector2i
    Width: uint16
    Constructor: Vector2i->State->State
}


[<JavaScript>]
module Rain =
    let Key = 4

    let GetEmitters (state:State) : list<RainEmitter> =
        GetList Key state

    let rand = Random()

    let private rain (state:State) emitter =
        emitter.Constructor
            {emitter.Corner with X = emitter.Corner.X + rand.Next(int emitter.Width)}
            state

    let private handleTick (effect:TickEffect) (state:State) =
        let state = GetEmitters(state)
                    |> List.fold (rain) state
        effect, state

    let SetEmitters (emitters:list<RainEmitter>) (state:State) =
        SetState
            Key
            emitters
            (Effects.Register TickEffect.TypeID handleTick 1)
            (Effects.Unregister<TickEffect> TickEffect.TypeID 1)
            state

    let Make emitter (state:State) =
        if emitter.Width = 0us then
            state
        else
            SetEmitters (emitter :: GetEmitters state) state


[<JavaScript>]
module Client =
    // Since IE does not support canvas natively, initialization of the 
    // canvas element is done through the excanvas.js library.
    [<Inline "G_vmlCanvasManager.initElement($elem)">]
    let Initialize (elem: CanvasElement) = ()

    [<Inline "requestAnimationFrame($0)">]
    let render (frame: unit->unit) = ()

    [<Inline """
        Math.trunc = Math.trunc || function(x) {
            return x < 0? Math.ceil(x): Math.floor(x);
        }
    """>]
    let inline truncPolyfill () = ()
    
    [<Inline "Math.trunc($0)">] // hack for Websharper's missing support for uint16 cast
    let toUint16 a = uint16 a

    let tileSize = 5
    let tileSizeF = float tileSize
    let cameraW = 80
    let cameraH = 40

    let draw (context: CanvasRenderingContext2D) (state:State) =
        let canvas = As<CanvasElement> context.Canvas
        context.ClearRect(0., 0., float canvas.Width, float canvas.Height)
        context.StrokeRect(0., 0., float canvas.Width, float canvas.Height)

        context.FillStyle <- "blue"
        for position in Water.GetPositions(state) do
            context.FillRect(float <| tileSize * position.X,
                             float <| tileSize * position.Y,
                             tileSizeF, tileSizeF)

        context.FillStyle <- "brown"
        for position in Floors.GetPositions(state) do
            context.FillRect(float <| tileSize * position.X,
                             float <| tileSize * position.Y,
                             tileSizeF, tileSizeF)

        context.FillStyle <- "lightgray"
        for x in 0 .. cameraW do
            match Bedrock.GetElevation x state with
            | None -> ()
            | Some y -> context.FillRect(float <| tileSize * x, float <| tileSize * y,
                                         tileSizeF, float <| canvas.Height - y)
        
    let Main () =
        truncPolyfill()

        let state = EmptyState
                    |> Rain.Make {Corner={X = 0; Y = 0}
                                  Width=(toUint16 cameraW)
                                  Constructor=Water.Make}
                    |> Rain.Make {Corner={X = 0; Y = -1}
                                  Width=(toUint16 cameraW)
                                  Constructor=Floors.Make}
                    |> ref
                    
        let rand = Random()
        for x in 0 .. cameraW do
            state := Bedrock.Make {X = x; Y = rand.Next(cameraH * 3 / 4, cameraH)} !state
            
        let element = Canvas [Attr.Style "display: inline;"]
        let canvas  = As<CanvasElement> element.Dom
        // Conditional initialization for the case of IE.
        if (canvas |> JS.Get "getContext" = JS.Undefined) then
            Initialize canvas
        canvas.Height <- cameraH * tileSize
        canvas.Width <- cameraW * tileSize
        
        let frame () =
            draw (canvas.GetContext "2d") !state

        let rec update () =
            JS.SetTimeout update 100 |> ignore

            let effect = TickEffect.Default
            let effect, newState = Effects.Trigger TickEffect.TypeID effect !state
            state := newState

            render frame

        update()

        element


[<AutoOpen>]
module Controls =
    type ParticleControl() =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body = Client.Main() :> _

