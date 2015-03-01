namespace Experiment

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.Html.Client


type State = Map<int, obj>


type Vector2i = {X:int ; Y:int}


[<JavaScript>]
module Water =
    let Key = 1

    let GetPositions (state:State) =
        match state.TryFind Key with
        | None -> Set.empty
        | Some positions -> positions :?> Set<Vector2i>

    let SetPositions positions (state:State) =
        state.Add(Key, positions)

    let Make (position:Vector2i) (state:State) =
        SetPositions (GetPositions(state).Add(position)) state
        
    let private fall (st:State) position =
        let positions = GetPositions(st)
        let positions = positions.Remove(position).Add({position with Y = position.Y + 1})
        SetPositions positions st

    let Update (state:State) =
        GetPositions(state)
        |> Set.fold fall state


module Camera =
    let Key = 2


[<JavaScript>]
module Client =
    
    // Since IE does not support canvas natively. Initialization of the 
    // canvas element is done through the excanvas.js library.
    [<Inline "G_vmlCanvasManager.initElement($elem)">]
    let Initialize (elem: CanvasElement) : unit = ()

    [<Inline "requestAnimationFrame($0)">]
    let render (frame: unit->unit) = X<unit>
        
    let Main () =
        let state = ref (Map.empty
                         |> Water.Make {X=1; Y=1}
                         |> Water.Make {X=3; Y=2}
                         |> Water.Make {X=3; Y=1}
                         |> Water.Make {X=3; Y=3})

        let drawStuff (context: CanvasRenderingContext2D) =
            let canvas = As<CanvasElement> context.Canvas
            context.ClearRect(0., 0., float canvas.Width, float canvas.Height)
            context.StrokeRect(0., 0., float canvas.Width, float canvas.Height)

            context.FillStyle <- "blue"
            for position in Water.GetPositions(!state) do
                let size = 5.
                context.FillRect(size * float position.X, size * float position.Y, size, size)
            
        let element = Canvas [Attr.Style "margin: 0 auto;"]
        let canvas  = As<CanvasElement> element.Dom
        // Conditional initialization for the case of IE.
        if (JS.Get "getContext" canvas = JS.Undefined) then
            Initialize canvas
        canvas.Height <- 200
        canvas.Width  <- 200

        let update () =
            state := !state
                     |> Water.Update
        JS.SetInterval update 100 |> ignore

        let rec frame () =
            render frame
            drawStuff (canvas.GetContext "2d")
        render frame

        element


type CanvasViewer() =
    inherit Web.Control()
    [<JavaScript>]
    override this.Body = Client.Main () :> _

