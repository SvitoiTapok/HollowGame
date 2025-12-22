module Program
open Raylib_cs
open System.Numerics
open Animation
open Camera
open MainLoop
open PhysicsEngine
open GameState
open GameObjectLoader


let loadAnimation = 
    let animation = 
        [| "resources/2.png";  |]
        |> fun frames -> loadAnimation frames 10
    Map.add "Attack" animation Map.empty

let toBool (x: CBool) : bool = x = CBool.op_Implicit true

    
let rec doFrame objects camera fpsCount =
    Raylib.BeginDrawing()
    Raylib.ClearBackground Raylib_cs.Color.White
    
    //drawGraphicObject sprite camera
    let bodies = nextFrame objects (1.0/fpsCount) camera
    Raylib.DrawText("Sprite Animation Demo", 10, 10, 20, Color.Black)
    
    Raylib.EndDrawing()
    // match toBool (Raylib.WindowShouldClose()) with
    // | false -> doFrame bodies camera fpsCount
    // | _ -> ()

let rec menuLoop camera = ()


let rec doGameLoop gameMode camera objects fpsCount =
    if toBool (Raylib.WindowShouldClose()) then
        ()
    else
        doFrame objects camera fpsCount
        
[<EntryPoint>]
let main argv =
    let fpsCount = 60.0
    Raylib.InitWindow(1500, 1000, "Hollow Game")
    Raylib.SetTargetFPS (int fpsCount)
    
    // Загрузка анимации (замените пути на реальные файлы)
    let map = loadAnimation
    let camera = newMovableDepthCamera 0 0 1500 1000 0.001f 0.001f 0.0f 0.0f

    

    
    doFrame (LoadGameObjects map) camera fpsCount
    Raylib.CloseWindow()
    0
