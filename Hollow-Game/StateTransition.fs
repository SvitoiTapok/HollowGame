module StateTransition
open Raylib_cs
open Animation
open InputHandler
open GameState
open System.Numerics
open Button



let menuButtonToRect button = 
    Rectangle(float32 button.Bounds.Point.X, float32 button.Bounds.Point.Y, float32 button.Bounds.W, float32 button.Bounds.H)


let handleTransition currentState buttons event =
    match currentState, event with
    | Menu, MouseClick(x, y, button) ->
        // Проверяем клик по элементам меню
        let clickedItem = 
            buttons
            |> List.tryFind (fun item -> toBool (Raylib.CheckCollisionPointRec(Vector2(float32 x, float32 y), menuButtonToRect item)))
        printfn "%A" clickedItem
        match clickedItem with
        | Some item ->
            match item.ButtonType with
            | StartGame -> 
                MainLoop
            | Exit ->
                currentState
        | None -> currentState
    | _ -> currentState
    // | Gameplay, KeyPressed key when key = int KeyboardKey.Escape ->
    //     // Пауза при нажатии ESC
    //     { currentState with 
    //         CurrentMode = PauseMenu
    //         PreviousMode = Some Gameplay }
    
    // | PauseMenu, MenuAction ResumeGame ->
    //     // Возврат из паузы
    //     { currentState with 
    //         CurrentMode = Gameplay
    //         PreviousMode = Some PauseMenu }
    
    // | Gameplay, Collision("player", "enemy") ->
    //     // Обработка столкновения
    //     { currentState with 
    //         CurrentMode = GameOver
    //         PreviousMode = Some Gameplay }
    
    // | Settings, MenuAction GoToMenu ->
    //     // Возврат в меню из настроек
    //     { currentState with 
    //         CurrentMode = MainMenu
    //         PreviousMode = Some Settings }
    
    // | _, _ -> currentState // Нет перехода для других комбинаций