module GameState
open Button
open Animation
open MainLoop
open InputHandler

type GameMode =
    | Menu
    | MainLoop
    | DeathScene
    | SelectionMode

type GameState = {
    GameMode: GameMode
    FpsCount: int
    Buttons: Button list
    Camera: Camera
    InputHandler: InputHandler
    GameObjects: GameObject list
}