open System
open Esquire


let left = Reactant 4
let width = Reactant 3
let right = left + width

right.Listen <| fun (prev, curr) -> Console.WriteLine("Changed right from " + string prev + " to " + string curr + ".")

let printStatus () =
    Console.WriteLine ()
    Console.WriteLine ("left: " + string left.Value)
    Console.WriteLine ("width: " + string width.Value)
    Console.WriteLine ("right: " + string right.Value)
    Console.WriteLine ()

printStatus()

Console.WriteLine "Changing left..."
left.Value <- -2
Console.WriteLine "Changed left."

printStatus()


let keyLeft = Reactant false
let keyShift = Reactant false
let alive = Reactant true
let onAttackWest = keyLeft.On(true).When(keyShift &&& alive)

keyLeft.Listen <| fun(prev, curr) -> Console.WriteLine (if curr then "Pressed left arrow key" else "Released left arrow key")
keyShift.Listen <| fun(prev, curr) -> Console.WriteLine (if curr then "Pressed shift" else "Released shift")
onAttackWest.Listen <| fun() -> Console.WriteLine "Attacked west"

keyLeft.Value <- true
keyShift.Value <- true

Console.WriteLine "Reassigning same value to keyLeft; this should not trigger listeners:"
keyLeft.Value <- true
Console.WriteLine "Done."

keyLeft.Value <- false
keyLeft.Value <- true


Console.WriteLine "Press Enter to continue..."
Console.ReadLine () |> ignore
