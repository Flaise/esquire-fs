namespace Esquire.Test

open Swensen.Unquote


[<AutoOpen>]
type MockFunctionExec<'a, 'b> (verifiers:array<'a->'b>) =
    let mutable calls = 0

    member this.ExpectCalls count =
        count <? verifiers.Length // use ExpectFinished instead
        calls =? count

    member this.ExpectFinished () =
        calls =? verifiers.Length

    member this.Call args =
        calls <? verifiers.Length
        calls <- calls + 1
        verifiers.[calls] args


[<AutoOpen>]
type MockFunctionExec2<'a, 'b, 'c> (verifiers:array<'a->'b->'c>) =
    let mutable calls = 0

    member this.ExpectCalls count =
        count <? verifiers.Length // use ExpectFinished instead
        calls =? count

    member this.ExpectFinished () =
        calls =? verifiers.Length

    member this.Call argA argB =
        calls <? verifiers.Length
        let index = calls
        calls <- calls + 1
        verifiers.[index] argA argB


[<AutoOpen>]
type MockFunction<'a when 'a:equality> (argLists:array<'a>) =
    let mutable calls = 0

    member this.ExpectCalls count =
        count <? argLists.Length // use ExpectFinished instead
        calls =? count

    member this.ExpectFinished () =
        calls =? argLists.Length

    member this.Call args =
        calls <? argLists.Length
        args =? argLists.[calls]
        calls <- calls + 1


[<AutoOpen>]
type MockFunction2<'a, 'b when 'a:equality and 'b:equality> (argLists:array<'a*'b>) =
    let mutable calls = 0

    member this.ExpectCalls count =
        count <? argLists.Length // use ExpectFinished instead
        calls =? count

    member this.ExpectFinished () =
        calls =? argLists.Length

    member this.Call argA argB =
        calls <? argLists.Length
        (argA, argB) =? argLists.[calls]
        calls <- calls + 1
