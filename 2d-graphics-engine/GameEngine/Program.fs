module Program
open Raylib_cs
open System.Numerics
open Animation


[<EntryPoint>]
let main argv =
    Raylib.InitWindow(800, 600, "F# Sprite Animation")
    Raylib.SetTargetFPS(60)
    
    // Загрузка анимации (замените пути на реальные файлы)
    let animation = 
        [| "resources/1.png"; "resources/2.jpg"; |]
        |> fun frames -> loadAnimation frames 10
    
    let map = Map.add "Attack" animation Map.empty
    let color = newColor 0uy 0uy 0uy 255uy
    let point = newPoint 100 100 0.0f
    let mutable sprite = newTextObject "Attack" (Raylib.GetFontDefault()) 100 (single 1.0) color point 0 TextAlign.Center
    //sprite <- newDrawableObject point 0 800 600 color map "Attack"
    let camera =  newMovableDepthCamera 0 0 800 600 0.001f 0.001f 0.0f 0.0f
    while not (Raylib.WindowShouldClose()) do
        // Обновление
        //sprite <- addPointToGraphicObjectPoint (updateGraphicObjectAnimation sprite) { X = 1; Y = 1; Z = 0.0f  } 

        Raylib.BeginDrawing()
        Raylib.ClearBackground Raylib_cs.Color.White
        
        drawGraphicObject sprite camera
        Raylib.DrawText("Sprite Animation Demo", 10, 10, 20, Color.Black)
        
        Raylib.EndDrawing()
    
    // Очистка
    //sprite.Animations.toArray |> Array.forall(fun el ->) |> Array.iter Raylib.UnloadTexture
    Raylib.CloseWindow()
    0
