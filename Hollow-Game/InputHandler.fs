module InputHandler

open Raylib_cs
open Animation

type InputEvent =
    | MouseClick of x: int * y: int * button: MouseButton
    | KeyPress of key: KeyboardKey
    | EscapePressed
    | SpacePressed
    | EnterPressed

type Character = {
    IsWalkingLeft: bool
    IsWalkingRight: bool
    IsInAir: bool
    IsJumping: bool
    IsAttacking: bool * Point
}
let toBool (x: CBool) : bool = x = CBool.op_Implicit true

type InputHandler() =
    
    let collectEvents() =
        let events = ResizeArray<InputEvent>()
        
        for i in 0..2 do
            let button = 
                match i with
                | 0 -> MouseButton.Left
                | 1 -> MouseButton.Right
                | 2 -> MouseButton.Middle
                | _ -> MouseButton.Left
            
            if toBool (Raylib.IsMouseButtonPressed(button)) then
                let pos = Raylib.GetMousePosition()
                events.Add(MouseClick(int pos.X, int pos.Y, button))
        
        if toBool (Raylib.IsKeyPressed(KeyboardKey.Escape)) then
            events.Add(EscapePressed)
        
        if toBool (Raylib.IsKeyPressed(KeyboardKey.Space)) then
            events.Add(SpacePressed)
        
        if toBool (Raylib.IsKeyPressed(KeyboardKey.Enter)) then
            events.Add(EnterPressed)
        for key in [KeyboardKey.W; KeyboardKey.A; KeyboardKey.S; KeyboardKey.D] do
            if toBool (Raylib.IsKeyDown(key)) then
                events.Add(KeyPress key)
        //printfn "%A" events
        events |> List.ofSeq
    
    member this.CollectEvents() = collectEvents()