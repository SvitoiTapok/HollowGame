module Program
open Raylib_cs
open System.Numerics
open Animation
open MainLoop
open PhysicsEngine
open GameState
open GameObjectLoader
open StateTransition
open InputHandler
open Button
open MenuCreator
open Enemy

let continueAttack attack = 
    let targetX = single attack.Target.X 
    let targetY = single attack.Target.Y
    let gameObjectX = single (getPointOfGraphicObject attack.GameObject.GraphicObject).X
    let gameObjectY = single (getPointOfGraphicObject attack.GameObject.GraphicObject).Y

    let newX = gameObjectX + (targetX - gameObjectX) * attack.Speed
    let newY = gameObjectY + (targetY - gameObjectY) * attack.Speed

    let p = newPoint (int newX) (int newY) 0.0f

    { attack with 
        GameObject = {attack.GameObject with GraphicObject = setPointToGraphicObjectPoint attack.GameObject.GraphicObject p }}

let attackHasCame attack =
    let targetX = single attack.Target.X 
    let targetY = single attack.Target.Y
    let gameObjectX = single (getPointOfGraphicObject attack.GameObject.GraphicObject).X
    let gameObjectY = single (getPointOfGraphicObject attack.GameObject.GraphicObject).Y

    let newX = gameObjectX + (targetX - gameObjectX) * attack.Speed
    let newY = gameObjectY + (targetY - gameObjectY) * attack.Speed
    let EPSILON = single 10
    abs (newX - targetX) < EPSILON && abs (newY - targetY) < EPSILON

let spawnAttack startPoint targetPoint w h animations currentAnimationName id speed= 
    let color = newColor 255uy 255uy 255uy 255uy
    {
        GameObject = makeGameObject startPoint 0 w h animations color currentAnimationName id "attackParticle" Static (v2 0.0 0.0) (v2 0.0 0.0)  (v2 0.0 0.0) InAir []
        Target = targetPoint
        Speed = speed
    }
let loadBaseAnimation () = 
    let animation = 
        [| "resources/startButton.png";  |]
        |> fun frames -> loadAnimation frames 10
    Map.add "base" animation Map.empty

let toBool (x: CBool) : bool = x = CBool.op_Implicit true

let downloadBackground () =
    let back = 
        [| "resources/background.png";  |]
        |> fun frames -> loadAnimation frames 1
    let background = Map.add "background" back Map.empty
    let color = newColor 255uy 255uy 255uy 255uy
    let point = newPoint -5000 -7000 10f
    let coliders=   [
            {
                Offset = v2 0.0 0.0
                Size = v2 100.0 100.0
                Kind = Solid
                Response = Ignore
                Name = "Ground"
            }
        ]
    makeGameObject point 4 30000 20000 background color "background" 2 "background" Static (v2 -5000 -7000) (v2 0.0 0.0) (v2 0.0 0.0) InAir coliders

let followingCamera (camera: Camera) (gameObject: GameObject) dSpeed = 
    let EPSILON = single 1e-2
    let point = getPointOfGraphicObject gameObject.GraphicObject
    let w, h = getSizesOfGraphicObject gameObject.GraphicObject

    let targetX = single point.X - single camera.W / 2.0f + single w / 2.0f
    let targetY = single point.Y - single camera.H / 2.0f + single h / 2.0f

    let newX = single camera.X + (targetX - single camera.X) * camera.Speed
    let newY = single camera.Y + (targetY - single camera.Y) * camera.Speed

    let isCame = abs (newX - targetX) < EPSILON && abs (newY - targetY) < EPSILON

    let newCamera = { 
        camera with 
            X = int newX
            Y = int newY
            Speed = if isCame then camera.initialSpeed else camera.Speed + dSpeed
    }

    newCamera
    

let handleEvents character Ilist =
    List.fold (fun acc item -> 
        match item with 
        | KeyPress key -> 
            match key with 
            | KeyboardKey.W -> {acc with IsJumping = true}
            | KeyboardKey.A -> {acc with IsWalkingLeft = true}
            | KeyboardKey.D -> {acc with IsWalkingRight = true}
            | _ -> acc
        | MouseClick(x, y, button) -> 
            { acc with IsAttacking = (true, newPoint x y 0.0f) }
        | _ -> acc) character Ilist 

let makePlayerWalkRight  character player = 
    if character.IsWalkingRight then 
        {player with 
            PhysicalObject = {player.PhysicalObject with speed={X = 1000.0; Y = player.PhysicalObject.speed.Y}}
            GraphicObject = setMirroredByHorizontal player.GraphicObject true}
    else player

let makePlayerWalkLeft  character player = 
    if character.IsWalkingLeft then 
        {player with 
            PhysicalObject = {player.PhysicalObject with speed={X = -1000.0; Y = player.PhysicalObject.speed.Y}}
            GraphicObject = setMirroredByHorizontal player.GraphicObject false}
    else player

let makePlayerAttack character player attacks = 
    let isAttacking, point = character.IsAttacking 
    if not isAttacking then attacks
    else
        List.append attacks [spawnAttack (getPointOfGraphicObject player.GraphicObject) point 100 100 (Map.add "default" AttackAnimation Map.empty) "default" 10000 0.05f]
let makePlayerJump  character player = 
    if character.IsJumping && not character.IsInAir then 
        {player with PhysicalObject = {player.PhysicalObject with speed={X = player.PhysicalObject.speed.X; Y = -1000.0}}}
    else player
let makePlayerMove player character attacks= 
    let player = makePlayerWalkRight character player |> makePlayerWalkLeft character |> makePlayerJump character
    let attacks = makePlayerAttack character player attacks
    let hasStoppedRunning = abs player.PhysicalObject.speed.X < 0.2 && getCurrentAnimation player.GraphicObject = "Run"
    let hasStartedRunning = abs player.PhysicalObject.speed.X > 0.2 && getCurrentAnimation player.GraphicObject = "Standing"
    
    if hasStoppedRunning then { player with GraphicObject = changeGameObjectAnimation player.GraphicObject "Standing" }, attacks
    else if hasStartedRunning then {player with GraphicObject = changeGameObjectAnimation player.GraphicObject "Run" }, attacks
    else player, attacks

// Обновление поведения всех врагов
let updateEnemies (enemies: GameObject list) (player: GameObject) dt =
    enemies
    |> List.map (fun enemy ->
        // Определяем тип врага (можно расширить для хранения типа в GameObject)
        let enemyType = 
            DefaultEnemy
        
        updateEnemy enemy player enemyType dt)

let doFrame gameState =
    let attacksGameObjs = gameState.Attacks |> List.map (fun x -> x.GameObject)
    // Получаем игрока
    let player = Option.get (List.tryFind (fun x -> x.PhysicalObject.name = "Player") gameState.GameObjects)
    
    // Разделяем врагов и другие объекты
    let enemies = gameState.GameObjects |> List.filter (fun x -> x.PhysicalObject.name.StartsWith("Enemy"))
    let otherObjects = gameState.GameObjects |> List.filter (fun x -> not (x.PhysicalObject.name.StartsWith("Enemy")))
    
    // Обновляем поведение врагов
    let updatedEnemies = updateEnemies enemies player (1.0 / float gameState.FpsCount)
    
    // Объединяем обновленные враги с другими объектами
    let allGameObjects = otherObjects @ updatedEnemies
    
    // Применяем физику ко всем объектам
    let bodies = nextFrame allGameObjects (1.0/float gameState.FpsCount) gameState.Camera
    
    // Обновляем игрока (существующий код)
    let player = Option.get (List.tryFind (fun x -> x.PhysicalObject.name = "Player") bodies)
    let attacks = gameState.Attacks
    let character = {
        IsWalkingLeft = false
        IsWalkingRight = false
        IsJumping = false
        IsInAir = if player.PhysicalObject.state=InAir then true else false
        IsAttacking = (false, newPoint 0 0 0.0f)
    }
    let newCharacter = gameState.InputHandler.CollectEvents() |> handleEvents character
    let (newPlayer: GameObject, attacks) = makePlayerMove player newCharacter attacks
    let camera = followingCamera gameState.Camera player 0.0001f
    {gameState with GameObjects = bodies |> List.map(fun x -> if x.PhysicalObject.name="Player" then newPlayer else x); Camera = camera; Attacks = attacks |> List.map continueAttack |> List.filter (fun d -> not (attackHasCame d))}


    //Raylib.DrawText("Sprite Animation Demo", 10, 10, 20, Color.Black)
    // match toBool (Raylib.WindowShouldClose()) with
    // | false -> doFrame bodies camera fpsCount
    // | _ -> ()

let setUpMenu gameState = 
    let buttonsGraph = gameState.Buttons |> List.map (fun x -> DrawableObject x.Bounds)
    let background = gameState.GameObjects |> List.map (fun x -> x.GraphicObject)

    drawAllVisibleObjects (List.concat [buttonsGraph; background] |> List.toArray) gameState.Camera
    let states = gameState.InputHandler.CollectEvents() |> List.map(fun x -> handleTransition gameState.GameMode gameState.Buttons x) |> List.filter(fun x -> not (x = gameState.GameMode))
    let state = if states |> List.length = 1 then states |> List.head else gameState.GameMode

    { gameState with GameMode = state }

// Функция для создания нескольких врагов на карте
let createEnemiesOnMap () =
    let enemyPositions = [
        v2 1000.0 0.0
        v2 2000.0 -100.0
        v2 3000.0 0.0
        v2 4000.0 -200.0
        v2 -1000.0 0.0
    ]
    
    // Создаем врагов разных типов
    let defaultEnemies = 
        enemyPositions.[0..2] 
        |> List.mapi (fun i pos -> 
            createEnemy pos DefaultEnemy (loadEnemyAnimation DefaultEnemy) (239932 + i))  
    
    
 
    defaultEnemies

let loadMainLoop gameState = 
    // Загружаем игровые объекты
    let baseGameObjects = LoadGameObjects (loadBaseAnimation ()) gameState.GameObjects
    
    // Создаем врагов
    let enemies = createEnemiesOnMap ()
    
    { gameState with 
        GameObjects = baseGameObjects @ enemies
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
            | MainLoop -> doFrame gameState
            | _ -> gameState
        
        doGameLoop newGameState
        
[<EntryPoint>]
let main argv =
    let fpsCount = 60.0
    Raylib.InitWindow(1500, 1000, "Hollow Game")
    Raylib.SetTargetFPS (int fpsCount)
    let InputHandler = InputHandler()
    
    // Загрузка анимации
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
        Attacks = List.empty
    }
    
    doGameLoop gameState
    Raylib.CloseWindow()
    0