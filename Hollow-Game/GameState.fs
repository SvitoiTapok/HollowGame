module GameState
open Button
open Animation
open MainLoop
open InputHandler

let AttackAnimation = [| "resources/kust.png" |] |> fun frames -> loadAnimation frames 1
        

type GameMode =
    | Menu
    | MainLoop
    | DeathScene
    | SelectionMode
    | LoadMainLoop
    | SetUpDeathScene
    | WinScene

type GameState = {
    GameMode: GameMode
    FpsCount: int
    Buttons: Button list
    Camera: Camera
    InputHandler: InputHandler
    GameObjects: GameObject list
}