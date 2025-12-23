module GameState
open Button
open Animation
open MainLoop
open InputHandler

type Attack = {
    GameObject: GameObject
    Target: Point
    Speed: single
}

let AttackAnimation = [| "resources/kust.png" |] |> fun frames -> loadAnimation frames 1
        

type GameMode =
    | Menu
    | MainLoop
    | DeathScene
    | SelectionMode
    | LoadMainLoop

type GameState = {
    GameMode: GameMode
    FpsCount: int
    Buttons: Button list
    Camera: Camera
    InputHandler: InputHandler
    GameObjects: GameObject list
    Attacks: Attack list
}