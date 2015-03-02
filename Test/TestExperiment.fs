namespace Esquire.Test

open NUnit.Framework
open Swensen.Unquote
open Microsoft.FSharp.Collections
open Experiment


module TestEffects =
    let state = EmptyState

    [<Test>]
    let ``call 1 handler`` () =
        let handler =
            MockFunctionExec2<int, State, int*State> [|
                fun a state ->
                    a =? 1
                    (a, state)
            |]

        let state = Effects.Register (handler.Call) 1 state
        handler.ExpectCalls 0

        let state = Effects.Trigger 1 state
        handler.ExpectFinished()

    [<Test>]
    let ``call 2 handlers in descending order`` () =
        let totalCalls = ref 0
        let handler1 =
            MockFunctionExec2<int, State, int*State> [|
                fun a state ->
                    a =? 1
                    !totalCalls =? 0
                    incr totalCalls
                    (a, state)
            |]
        let handler2 =
            MockFunctionExec2<int, State, int*State> [|
                fun a state ->
                    a =? 1
                    !totalCalls =? 1
                    incr totalCalls
                    (a, state)
            |]

        let state = Effects.Register handler1.Call 1 state
        handler1.ExpectCalls 0

        let state = Effects.Register handler2.Call 2 state
        handler2.ExpectCalls 0

        let state = Effects.Trigger 1 state
        handler1.ExpectFinished()
        handler2.ExpectFinished()

    [<Test>]
    let ``can't register two of the same type and priority`` () =
        let handler a b = a, b
        
        let state = Effects.Register handler 1 state

        Assert.Throws(fun () -> Effects.Register handler 1 state |> ignore)
        |> ignore
