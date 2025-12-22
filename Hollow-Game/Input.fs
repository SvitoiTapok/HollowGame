open Raylib_cs
open System.Numerics
open Animation 

module Input =
    let isMouseButtonClicked button =
        Raylib.IsMouseButtonPressed(button)

    let tryGetLeftMouseClickPosition() =
        if Raylib.IsMouseButtonPressed(MouseButton.Left) then
            let pos = Raylib.GetMousePosition()
            Some { X = pos.X; Y = pos.Y; Z = 0.0f }
        else
            None

    let tryGetRightMouseClickPosition() =
        if Raylib.IsMouseButtonPressed(MouseButton.Right) then
            let pos = Raylib.GetMousePosition()
            Some { X = pos.X; Y = pos.Y; Z = 0.0f }
        else
            None

    let tryGetMouseClickPosition button =
        if Raylib.IsMouseButtonPressed(button) then
            let pos = Raylib.GetMousePosition()
            Some { X = pos.X; Y = pos.Y; Z = 0.0f }
        else
            None

    let getLeftMouseClickPosition() =
        if Raylib.IsMouseButtonPressed(MouseButton.Left) then
            let pos = Raylib.GetMousePosition()
            { X = pos.X; Y = pos.Y; Z = 0.0f }
        else
            { X = 0.0f; Y = 0.0f; Z = 0.0f }

    let getAllMouseClicks() =
        [ MouseButton.Left; MouseButton.Right; MouseButton.Middle ]
        |> List.filter Raylib.IsMouseButtonPressed
        |> List.map (fun btn ->
            let pos = Raylib.GetMousePosition()
            (btn, { X = pos.X; Y = pos.Y; Z = 0.0f })
        )

    let isClickInRectangle rect button =
        if Raylib.IsMouseButtonPressed(button) then
            let pos = Raylib.GetMousePosition()
            Raylib.CheckCollisionPointRec(pos, rect)
        else
            false

    let isClickInCircle center radius button =
        if Raylib.IsMouseButtonPressed(button) then
            let mousePos = Raylib.GetMousePosition()
            Raylib.CheckCollisionPointCircle(mousePos, center, radius)
        else
            false

    let isKeyboardKeyClicked key =
        Raylib.IsKeyPressed(key)

    let isSpaceClicked() =
        isKeyboardKeyClicked KeyboardKey.Space

    let isEnterClicked() =
        isKeyboardKeyClicked KeyboardKey.Enter

    let isEscapeClicked() =
        isKeyboardKeyClicked KeyboardKey.Escape

    let isAnyKeyClicked keys =
        keys |> List.exists isKeyboardKeyClicked
