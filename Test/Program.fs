namespace Esquire.Test

open NUnit.Framework
open Swensen.Unquote
open Esquire

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
        let reactant = Reactant<int>(fun () -> 4)
        reactant.Value =? 4

    [<Test>]
    let ``set/retrieve value computation`` () =
        let reactant = Reactant 0
        reactant.Derive <| fun () -> 9
        reactant.Value =? 9

    [<Test>]
    let ``register and trigger callback`` () =
        let reactant = Reactant 7
        let called = ref false
        (reactant.Listen <| fun (prev, curr) ->
            !called =? false
            prev =? 7
            curr =? 9
            called := true
        ) |> ignore
        reactant.Value <- 9
        !called =? true

    [<Test>]
    let ``unregister callback`` () =
        let reactant = Reactant 9
        let registration = reactant.Listen <| fun (prev, curr) -> Assert.Fail ()
        registration.Remove()
        reactant.Value <- -2

    [<Test>]
    let ``on(value) filtering`` () =
        let reactant = Reactant 1
        let called = ref false
        (reactant.On(5).Listen <| fun () ->
            !called =? false
            called := true
        ) |> ignore
        reactant.Value <- 4
        !called =? false
        reactant.Value <- 5
        !called =? true
        reactant.Value <- 8

    [<Test>]
    let ``only trigger when value changes`` () =
        let reactant = Reactant 5
        let called = ref false
        (reactant.Listen <| fun (prev, curr) ->
            !called =? false
            prev =? 5
            curr =? 4
            called := true
        ) |> ignore
        reactant.Value <- 4
        reactant.Value <- 4

    [<Test>]
    let ``composition polling`` () =
        let a = Reactant true
        let b = Reactant true
        let ab = a &&& b
        ab.Value =? true

        a.Value <- false
        ab.Value =? false

        b.Value <- false
        a.Value <- true
        ab.Value =? false

        b.Value <- true
        ab.Value =? true
        
    [<Test>]
    let ``composition events`` () =
        let a = Reactant true
        let b = Reactant true
        let ab = a &&& b
        let calls = ref 0
        (ab.Listen <| fun (prev, curr) ->
            calls := !calls + 1
        ) |> ignore

        a.Value <- false
        !calls =? 1

        b.Value <- false
        !calls =? 1

        a.Value <- true
        !calls =? 1

        b.Value <- true
        !calls =? 2

    [<Test>]
    let ``compose dispatcher with reactant`` () =
        let dispatcher = EventDispatcher ()
        let reactant = Reactant false
        let composition = dispatcher.When reactant
        let calls = ref 0
        (composition.Listen <| fun () ->
            calls := !calls + 1
        ) |> ignore

        dispatcher.Trigger ()
        !calls =? 0

        reactant.Value <- true
        !calls =? 0

        dispatcher.Trigger ()
        !calls =? 1

        reactant.Value <- false
        !calls =? 1

        dispatcher.Trigger ()
        !calls =? 1
