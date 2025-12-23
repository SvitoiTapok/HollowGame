module WinSceneCreator
open Button
open Animation

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
