module MenuCreator
open Button
open Animation

let makeMenuButtons () = 
    let color = newColor 255uy 255uy 255uy 255uy
    let exitButton = 
        [| "resources/exitButton.png";  |]
        |> fun frames -> loadAnimation frames 1
    let exitButtonMap = Map.add "exitButton" exitButton Map.empty
    let startButton = 
        [| "resources/startButton.png";  |]
        |> fun frames -> loadAnimation frames 1
    let startButtonMap = Map.add "startButton" startButton Map.empty
    [
        {
            Bounds = { 
                Point = {
                    X = 700
                    Y = 100
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
        }
        {
            Bounds = { 
                Point = {
                    X = 700
                    Y = 500
                    Z = 0f
                }
                Layer = 0
                W = 300
                H = 140
                Animations = exitButtonMap
                Color = color
                CurrentAnimationName = "exitButton"
                MirroredByHorizontal = false
            }
            ButtonType = Exit
        }


    ]