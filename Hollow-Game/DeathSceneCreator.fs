module DeathSceneCreator
open Button
open Animation
open PhysicsEngine
open GameObjectLoader


let downloadDeathScene() =
    let d = 
        [| "resources/death_scene.png";  |]
        |> fun frames -> loadAnimation frames 1
    let dd = Map.add "deathScene" d Map.empty
    let color = newColor 255uy 255uy 255uy 255uy
    let point = newPoint 0 0 0f
    let coliders=   [
            {
                Offset = v2 0.0 0.0
                Size = v2 100.0 100.0
                Kind = Solid
                Response = Ignore
                Name = "Ground"
            }
        ]
    makeGameObject point 3 1500 1000 dd color "deathScene" 2 "deathScene" Static (v2 0 0) (v2 0.0 0.0) (v2 0.0 0.0) InAir coliders


let createDeathScene () = 
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
                    Y = 750
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
