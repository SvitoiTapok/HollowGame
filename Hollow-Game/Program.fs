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

let downloadBackground () =
    let back = 
        [| "resources/background.png";  |]
        |> fun frames -> loadAnimation frames 1
    let background = Map.add "background" back Map.empty
    let color = newColor 255uy 255uy 255uy 255uy
    let graphBack = { 
                Point = {
                    X = 0
                    Y = 0
                    Z = 4f
                }
                Layer = 4
                W = 8000
                H = 6000
                Animations = background
                Color = color
                CurrentAnimationName = "background"
            }
    {
        GraphicObject = DrawableObject graphBack
        PhysicalObject = {
            id = 2
            name = "background"
            bodyType = Static
            pos = v2 0 0
            speed = v2 0.0 0.0
            acc = v2 0.0 0.0
            state = InAir
            colliders =
                [
                    {
                        Offset = v2 0.0 0.0
                        Size = v2 100.0 100.0
                        Kind = Solid
                        Response = Ignore
                        Name = "Ground"
                    }
                ]
        }   
    }



let handleEvents character Ilist =
    List.fold (fun acc item -> 
        match item with 
        | KeyPress key -> 
            match key with 
            | KeyboardKey.W -> {acc with IsJumping = true}
            | KeyboardKey.A -> {acc with IsWalkingLeft = true}
            | KeyboardKey.D -> {acc with IsWalkingRight = true}
            | _ -> acc
        | _ -> acc) character Ilist 

let makePlayerWalkRight  character player = 
    if character.IsWalkingRight then {player with speed={X = 1000.0; Y = player.speed.Y}}
    else player
let makePlayerWalkLeft  character player = 
    if character.IsWalkingLeft then {player with speed={X = -1000; Y = player.speed.Y}}
    else player

let makePlayerJump  character player = 
    if character.IsJumping && not character.IsInAir then {player with speed={X = player.speed.X; Y = -1000}}
    else player
let makePlayerMove player character= 
    printfn "%A" character.IsWalkingRight
    makePlayerWalkRight character player |> makePlayerWalkLeft character |> makePlayerJump character

    

let rec doFrame gameState =
    
    //drawGraphicObject sprite camera
    let bodies = nextFrame gameState.GameObjects (1.0/float gameState.FpsCount) gameState.Camera
    printfn "%A" gameState
    let player = Option.get (List.tryFind (fun x -> x.PhysicalObject.name = "Player") bodies)
    let character = {
        IsWalkingLeft = false
        IsWalkingRight = false
        IsJumping = false
        IsInAir = if player.PhysicalObject.state=InAir then true else false
    }
    let newCharacter = gameState.InputHandler.CollectEvents() |> handleEvents character
    let newPlayer = makePlayerMove player.PhysicalObject newCharacter
    bodies |> List.map(fun x -> if x.PhysicalObject.name="Player" then {x with PhysicalObject=newPlayer} else x)


    //Raylib.DrawText("Sprite Animation Demo", 10, 10, 20, Color.Black)
    // match toBool (Raylib.WindowShouldClose()) with
    // | false -> doFrame bodies camera fpsCount
    // | _ -> ()

let setUpMenu gameState = 
    //printfn "%A" gameState
    let buttonsGraph = gameState.Buttons |> List.map (fun x -> DrawableObject x.Bounds)
    let background = gameState.GameObjects |> List.map (fun x -> x.GraphicObject)

    drawAllVisibleObjects ( List.concat [buttonsGraph; background]|> List.toArray) gameState.Camera
    let states = gameState.InputHandler.CollectEvents() |> List.map(fun x -> handleTransition gameState.GameMode gameState.Buttons x) |> List.filter(fun x -> not (x = gameState.GameMode))
    let state = if states |> List.length=1 then states |> List.head else gameState.GameMode


    {gameState with GameMode = state}

let loadMainLoop gameState = 
    { gameState with 
        GameObjects = LoadGameObjects (loadBaseAnimation ()) gameState.GameObjects
        GameMode = MainLoop 
    }

let rec doGameLoop gameState =
    if toBool (Raylib.WindowShouldClose()) then
        ()
    else
        let newGameState =
         match gameState.GameMode with
            | Menu -> setUpMenu gameState
            | LoadMainLoop -> loadMainLoop gameState
            | MainLoop -> {gameState with GameObjects= doFrame gameState}
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
        GameObjects = [downloadBackground ()]
        InputHandler = InputHandler
    }
    

    doGameLoop gameState
    //doFrame  camera fpsCount
    Raylib.CloseWindow()
    0
