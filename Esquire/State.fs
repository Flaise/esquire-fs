namespace Esquire


type State<'a> (value:'a, ?listeners:list<'a->'a->unit>) =
    member this.Listeners = match listeners with
                            | None -> []
                            | Some listeners -> listeners
    member this.Value = value


[<AutoOpen>]
module States =
    let update (value:'a) (st:State<'a>) =
        if value = st.Value then
            st
        else
            for listener in st.Listeners do
                listener st.Value value
            State (value, st.Listeners)

    let listen (listener:'a->'a->unit) (st:State<'a>) =
        State (st.Value, listener :: st.Listeners)

