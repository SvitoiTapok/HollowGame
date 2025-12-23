module WinSceneCreator
open Button
open Animation
open GameObjectLoader
open PhysicsEngine


let downloadWinScene() =
    let d = 
        [| "resources/win_scene.png";  |]
        |> fun frames -> loadAnimation frames 1
    let dd = Map.add "win_scene" d Map.empty
    let color = newColor 255uy 255uy 255uy 255uy
    let point = newPoint 0 0 0f
    let coliders=   []
    makeGameObject point 3 1500 1000 dd color "win_scene" 2 "win_scene" Static (v2 0 0) (v2 0.0 0.0) (v2 0.0 0.0) InAir coliders
let createWinScene () = 
    let color = newColor 255uy 255uy 255uy 255uy
    let startButton = 
        [| "resources/startButton.png";  |]
        |> fun frames -> loadAnimation frames 1
    let startButtonMap = Map.add "startButton" startButton Map.empty
    [
        {
            Bounds = { 
                Point = {
                    X = 630
                    Y = 800
                    Z = 0f
                }
                Layer = 0
                W = 300
                H = 140
                Animations = startButtonMap
                Color = color
                CurrentAnimationName = "startButton"
                MirroredByHorizontal = false
            }
            ButtonType = StartGame
        }]
