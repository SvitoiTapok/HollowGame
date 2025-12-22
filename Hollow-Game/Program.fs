module Program
open Raylib_cs
open System.Numerics
open Animation
open Camera
open MainLoop
open PhysicsEngine
open GameState
open GameObjectLoader
open StateTransition
open InputHandler
open Button
open MenuCreator


let loadBaseAnimation () = 
    let animation = 
        [| "resources/startButton.png";  |]
        |> fun frames -> loadAnimation frames 10
    Map.add "Attack" animation Map.empty

let toBool (x: CBool) : bool = x = CBool.op_Implicit true

    
let rec doFrame objects camera fpsCount =
    
    //drawGraphicObject sprite camera
    let bodies = nextFrame objects (1.0/fpsCount) camera
    //Raylib.DrawText("Sprite Animation Demo", 10, 10, 20, Color.Black)
    bodies
    // match toBool (Raylib.WindowShouldClose()) with
    // | false -> doFrame bodies camera fpsCount
    // | _ -> ()

let setUpMenu gameState = 
    //printfn "%A" gameState
    drawAllVisibleObjects (gameState.Buttons |> List.map (fun x -> DrawableObject x.Bounds) |> List.toArray) gameState.Camera
    let states = gameState.InputHandler.CollectEvents() |> List.map(fun x -> handleTransition gameState.GameMode gameState.Buttons x) |> List.filter(fun x -> not (x = gameState.GameMode))
    let state = if states |> List.length=1 then states |> List.head else gameState.GameMode
    printf "%A" state
    //gameState.Buttons |> List.map(fun x -> setPointToGraphicObjectPoint (DrawableObject x.Bounds) {X=x.Bounds.Point.X; Y=x.Bounds.Point.Y; Z=x.Bounds.Point.Z} )
    
    {gameState with GameMode = state}


let rec doGameLoop gameState =
    if toBool (Raylib.WindowShouldClose()) then
        ()
    else
        let newGameState =
         match gameState.GameMode with
            | Menu -> setUpMenu gameState
            | MainLoop -> {gameState with GameObjects= doFrame gameState.GameObjects gameState.Camera gameState.FpsCount}
            | _ -> gameState
        doGameLoop newGameState
        
[<EntryPoint>]
let main argv =
    let fpsCount = 60.0
    Raylib.InitWindow(1500, 1000, "Hollow Game")
    Raylib.SetTargetFPS (int fpsCount)
    let InputHandler = InputHandler()
    
    // Загрузка анимации (замените пути на реальные файлы)
    let map = loadBaseAnimation ()
    let camera = newMovableDepthCamera 0 0 1500 1000 0.001f 0.001f 1000f 0.0f

    let buttons = makeMenuButtons ()
    let gameState = {
        GameMode = Menu;
        FpsCount = int fpsCount;
        Buttons = buttons
        Camera = camera
        GameObjects = LoadGameObjects map
        InputHandler = InputHandler
    }
    

    doGameLoop gameState
    //doFrame  camera fpsCount
    Raylib.CloseWindow()
    0
