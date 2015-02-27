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
        ab.Listen (fun (prev, curr) -> incr calls) |> ignore

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
        composition.Listen (fun () -> incr calls) |> ignore

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

    [<Test>]
    let ``transformation`` () =
        let source = Reactant 4
        let result = source.Transform <| fun a -> a > 4
        result.Value =? false
        let calls = ref 0

        (result.Listen <| fun (prev, curr) ->
            match !calls with
            | 0 ->
                prev =? false
                curr =? true
            | 1 ->
                prev =? true
                curr =? false
            | _ ->
                Assert.Fail ()
            incr calls
        ) |> ignore

        source.Value <- 5
        !calls =? 1
        result.Value =? true

        source.Value <- 6
        !calls =? 1
        result.Value =? true

        source.Value <- 3
        !calls =? 2
        result.Value =? false

    [<Test>]
    let ``event composition with self`` () =
        let reactant = Reactant 1
        let composition = reactant.When <| reactant.Transform (fun a -> a > 5)
        let calls = ref 0
        (composition.Listen <| fun (prev, curr) ->
            match !calls with
            | 0 ->
                prev =? 1
                curr =? 6
            | 1 ->
                prev =? 2
                curr =? 10
            | 2 ->
                prev =? 10
                curr =? 12
            | 3 ->
                prev =? 12
                curr =? 6
            | 4 ->
                prev =? 5
                curr =? 6
            | _ ->
                Assert.Fail ()
            incr calls
        ) |> ignore

        reactant.Value <- 6
        !calls =? 1
        reactant.Value <- 3
        !calls =? 1
        reactant.Value <- 2
        !calls =? 1
        reactant.Value <- 10
        !calls =? 2
        reactant.Value <- 12
        !calls =? 3
        reactant.Value <- 6
        !calls =? 4
        reactant.Value <- 5
        !calls =? 4
        reactant.Value <- 6
        !calls =? 5
