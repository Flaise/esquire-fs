namespace Esquire.Test

open NUnit.Framework
open Swensen.Unquote
open Esquire
open Microsoft.FSharp.Collections


type MockFunction<'a when 'a:equality> (argLists:array<'a>) =
    let mutable calls = 0

    member this.ExpectCalls count =
        count <? argLists.Length
        calls =? count

    member this.ExpectFinished () =
        calls =? argLists.Length

    member this.Call args =
        calls <? argLists.Length
        args =? argLists.[calls]
        calls <- calls + 1

type MockFunction2<'a, 'b when 'a:equality and 'b:equality> (argLists:array<'a*'b>) =
    let mutable calls = 0

    member this.ExpectCalls count =
        count <? argLists.Length
        calls =? count

    member this.ExpectFinished () =
        calls =? argLists.Length

    member this.Call argA argB =
        calls <? argLists.Length
        (argA, argB) =? argLists.[calls]
        calls <- calls + 1
        

module TestState =
    let add value (st:State<Set<'a>>): State<Set<'a>> =
        update (st.Value.Add(value)) st

    [<Test>]
    let ``construct with value`` () =
        let state = State 1
        state.Value =? 1

    [<Test>]
    let ``notify listeners on update`` () =
        let mockFunc = MockFunction2 [| (1, 2) |]
        State 1
        |> listen mockFunc.Call
        |> update 2
        |> ignore
        mockFunc.ExpectFinished ()

    [<Test>]
    let ``notify listeners of complex state update`` () =
        let mockFunc = MockFunction2 [| (Set.empty, Set.empty.Add(1)) |]
        let state = State Set.empty
        let state = listen mockFunc.Call state
        let state = add 1 state
        state.Value =? Set.empty.Add(1)
        mockFunc.ExpectFinished ()


module TestReactant =
    [<Test>]
    let ``constructed with value`` () =
        let reactant = Reactant 2
        reactant.Value =? 2

    [<Test>]
    let ``set/retrieve simple value`` () =
        let reactant = Reactant 1
        reactant.Value <- 5
        reactant.Value =? 5

    [<Test>]
    let ``constructed with computed value`` () =
        let reactant = Reactant<int> (fun () -> 4)
        reactant.Value =? 4

    [<Test>]
    let ``set/retrieve value computation`` () =
        let reactant = Reactant 0
        reactant.Derive <| fun () -> 9
        reactant.Value =? 9

    [<Test>]
    let ``register and trigger callback`` () =
        let reactant = Reactant 7
        let mockFunc = MockFunction [| (7, 9) |]
        reactant.Listen mockFunc.Call |> ignore
        reactant.Value <- 9
        mockFunc.ExpectFinished ()

    [<Test>]
    let ``unregister callback`` () =
        let reactant = Reactant 9
        let registration = reactant.Listen <| fun (prev, curr) -> Assert.Fail ()
        registration.Remove()
        reactant.Value <- -2

    [<Test>]
    let ``on(value) filtering`` () =
        let reactant = Reactant 1
        let mockFunc = MockFunction [| () |]
        reactant.On(5).Listen mockFunc.Call |> ignore

        reactant.Value <- 4
        mockFunc.ExpectCalls 0

        reactant.Value <- 5
        mockFunc.ExpectFinished ()

        reactant.Value <- 8

    [<Test>]
    let ``only trigger when value changes`` () =
        let reactant = Reactant 5
        let mockFunc = MockFunction [| (5, 4) |]
        reactant.Listen mockFunc.Call |> ignore

        reactant.Value <- 4
        mockFunc.ExpectFinished ()

        reactant.Value <- 4
        
    [<Test>]
    let ``composition events`` () =
        let a = Reactant true
        let b = Reactant true
        let ab = a &&& b

        let mockFunc = MockFunction [| (true, false); (false, true) |]
        ab.Listen mockFunc.Call |> ignore

        a.Value <- false
        mockFunc.ExpectCalls 1

        b.Value <- false
        mockFunc.ExpectCalls 1

        a.Value <- true
        mockFunc.ExpectCalls 1

        b.Value <- true
        mockFunc.ExpectFinished ()

    [<Test>]
    let ``compose dispatcher with reactant`` () =
        let dispatcher = EventDispatcher ()
        let reactant = Reactant false
        let composition = dispatcher.When reactant
        let mockFunc = MockFunction [| () |]
        composition.Listen mockFunc.Call |> ignore

        dispatcher.Trigger ()
        mockFunc.ExpectCalls 0

        reactant.Value <- true
        mockFunc.ExpectCalls 0

        dispatcher.Trigger ()
        mockFunc.ExpectFinished ()

        reactant.Value <- false
        mockFunc.ExpectFinished ()

        dispatcher.Trigger ()
        mockFunc.ExpectFinished ()

    [<Test>]
    let ``transformation`` () =
        let source = Reactant 4
        let result = source.Transform <| fun a -> a > 4
        result.Value =? false
        let mockFunc = MockFunction [| (false, true); (true, false) |]
        result.Listen mockFunc.Call |> ignore

        source.Value <- 5
        mockFunc.ExpectCalls 1
        result.Value =? true

        source.Value <- 6
        mockFunc.ExpectCalls 1
        result.Value =? true

        source.Value <- 3
        mockFunc.ExpectFinished ()
        result.Value =? false

    [<Test>]
    let ``event composition with self`` () =
        let reactant = Reactant 1
        let composition = reactant.When <| reactant.Transform (fun a -> a > 5)
        let mockFunc = MockFunction [| (1, 6); (2, 10); (10, 12); (12, 6); (5, 6) |]
        composition.Listen mockFunc.Call |> ignore

        reactant.Value <- 6
        mockFunc.ExpectCalls 1
        reactant.Value <- 3
        mockFunc.ExpectCalls 1
        reactant.Value <- 2
        mockFunc.ExpectCalls 1
        reactant.Value <- 10
        mockFunc.ExpectCalls 2
        reactant.Value <- 12
        mockFunc.ExpectCalls 3
        reactant.Value <- 6
        mockFunc.ExpectCalls 4
        reactant.Value <- 5
        mockFunc.ExpectCalls 4
        reactant.Value <- 6
        mockFunc.ExpectFinished ()

