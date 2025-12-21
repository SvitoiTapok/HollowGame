module Program
open Raylib_cs
open System.Numerics
open Animation
open Camera
open MainLoop
open PhysicsEngine


[<EntryPoint>]
let main argv =
    Raylib.InitWindow(1500, 1000, "F# Sprite Animation")
    Raylib.SetTargetFPS(60)
    
    // Загрузка анимации (замените пути на реальные файлы)
    let animation = 
        [| "resources/2.png";  |]
        |> fun frames -> loadAnimation frames 10
    
    let map = Map.add "Attack" animation Map.empty
    let color = newColor 255uy 255uy 255uy 255uy
    let mutable sprite1 = DrawableObject {
        Point = {
            X = 0
            Y = 0
            Z = 0f
        }
        Layer = 0
        W = 400
        H = 300
        Animations = map
        Color = color
        CurrentAnimationName = "Attack"
    }
    let mutable sprite2 = DrawableObject {
        Point = {
            X = 0
            Y = 0
            Z = 0f
        }
        Layer = 0
        W = 400
        H = 300
        Animations = map
        Color = color
        CurrentAnimationName = "Attack"
    }
    let mutable platform = {
        id = 1
        bodyType = Static
        pos = v2 400.0 360.0
        speed = v2 0.0 0.0
        acc = v2 0.0 0.0
        state = InAir
        colliders =
            [
                {
                    Offset = v2 0.0 0.0
                    Size = v2 400.0 300.0
                    Kind = Solid
                    Response = Block
                    Name = "Ground"
                }
            ]
    }
    let mutable body = {
        id = 2
        bodyType = Dynamic
        pos = v2 0.0 0.0
        speed = v2 1.0 0.0
        acc = v2 1.0 0.0
        state = InAir
        colliders =
            [
                {
                    Offset = v2 0.0 0.0
                    Size = v2 400.0 300.0
                    Kind = Solid
                    Response = Block
                    Name = "Player"
                }
            ]
    }
    let obj1 = {
        GraphicObject = sprite1
        PhysicalObject = platform
    }
    let obj2 = {
        GraphicObject = sprite2
        PhysicalObject = body
    }
    let camera = newMovableDepthCamera 0 0 1500 1000 0.001f 0.001f 0.0f 0.0f
    //while not (Raylib.WindowShouldClose()) do
    let rec doFrame objects =
        // Обновление
        // sprite <- addPointToGraphicObjectPoint (updateGraphicObjectAnimation sprite) { X = 1; Y = 1; Z = 0.0f  } 

        Raylib.BeginDrawing()
        Raylib.ClearBackground Raylib_cs.Color.White
        
        //drawGraphicObject sprite camera
        let bodies = nextFrame objects (1.0/60.0) camera



        Raylib.DrawText("Sprite Animation Demo", 10, 10, 20, Color.Black)
        
        Raylib.EndDrawing()
        doFrame bodies
    
    doFrame [obj1; obj2]
    // Очистка
    //sprite.Animations.toArray |> Array.forall(fun el ->) |> Array.iter Raylib.UnloadTexture
    Raylib.CloseWindow()
    0
