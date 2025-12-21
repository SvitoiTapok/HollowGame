open Rectangle
open Camera 
open Graphic
open Example
open Raylib_cs

let updateState (state: GameState) : GameState =
    state

let rec gameLoop (state: GameState) =
    if toBool (Raylib.WindowShouldClose()) || toBool (Raylib.IsKeyPressed KeyboardKey.Escape) then
        ()
    else
        //drawRectangles state.Rectangles
        let state' = updateState state
        gameLoop state'


[<EntryPoint>]
let main _ =
    Raylib.InitWindow (800, 600, "Сосать Америка")
    Raylib.SetTargetFPS 60
    Raylib.SetExitKey KeyboardKey.Escape 

    let rect1 = { 
        X = 300.0f
        Y = 225.0f
        W = 200
        H = 150
        Color = Color(51uy, 179uy, 76uy, 255uy) 
    }

    let rect2 = { 
        X = 400.0f
        Y = 400.0f
        W = 100
        H = 100
        Color = Color(10uy, 129uy, 176uy, 255uy) 
    }

    let randomRects = generateRectangles 10 (single 5000) (single 5000) 10 1000
        
    loop rect1 {X=rect2.X; Y=rect2.Y; W=800; H=600; Speed=single 0; initialSpeed=single 0} randomRects

    Raylib.CloseWindow()
    0