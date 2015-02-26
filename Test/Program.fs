namespace Esquire.Test

open NUnit.Framework
open Swensen.Unquote
open Esquire

module TestReactant =
    [<Test>]
    let ``constructed with value`` () =
        let reactant = Reactant 2
        test <@ reactant.Value = 2 @>

    [<Test>]
    let ``set/retrieve simple value`` () =
        let reactant = Reactant 1
        reactant.Value <- 5
        test <@ reactant.Value = 5 @>

    [<Test>]
    let ``constructed with computed value`` () =
        let reactant = Reactant<int>(fun () -> 4)
        test <@ reactant.Value = 4 @>

    [<Test>]
    let ``set value computation`` () =
        let reactant = Reactant 0
        reactant.Derive <| fun () -> 9
        test <@ reactant.Value = 9 @>
