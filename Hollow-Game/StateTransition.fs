module StateTransition
open Raylib_cs
open Animation

type MenuAction = 
    | StartGame
    | Exit

type MenuButton = {
    Bounds: DrawableObject
    Action: MenuAction
}



let menuButtonToRect button = 
    Rectangle(float32 button.Bounds.Point.X, float32 button.Bounds.Point.Y, float32 button.Bounds.W, float32 button.Bounds.H)


// let handleTransition currentState buttons event : GameState =
//     match currentState.CurrentMode, event with
//     | MainMenu, MouseClick(x, y, button) ->
//         // Проверяем клик по элементам меню
//         let clickedItem = 
//             buttons
//             |> List.tryFind (fun item -> Raylib.CheckCollisionPointRec(Vector2(float32 x, float32 y), item.Bounds))
        
//         match clickedItem with
//         | Some item ->
//             match item.Action with
//             | StartGame -> 
//                 { currentState with 
//                     CurrentMode = Gameplay
//                     PreviousMode = Some MainMenu
//                     Level = 1 }
//             | GoToSettings ->
//                 { currentState with 
//                     CurrentMode = Settings
//                     PreviousMode = Some MainMenu }
//             | ExitGame ->
//                 // Флаг для выхода из игры
//                 { currentState with ShouldExit = true }
//             | _ -> currentState
//         | None -> currentState
    
//     | Gameplay, KeyPressed key when key = int KeyboardKey.Escape ->
//         // Пауза при нажатии ESC
//         { currentState with 
//             CurrentMode = PauseMenu
//             PreviousMode = Some Gameplay }
    
//     | PauseMenu, MenuAction ResumeGame ->
//         // Возврат из паузы
//         { currentState with 
//             CurrentMode = Gameplay
//             PreviousMode = Some PauseMenu }
    
//     | Gameplay, Collision("player", "enemy") ->
//         // Обработка столкновения
//         { currentState with 
//             CurrentMode = GameOver
//             PreviousMode = Some Gameplay }
    
//     | Settings, MenuAction GoToMenu ->
//         // Возврат в меню из настроек
//         { currentState with 
//             CurrentMode = MainMenu
//             PreviousMode = Some Settings }
    
//     | _, _ -> currentState // Нет перехода для других комбинаций