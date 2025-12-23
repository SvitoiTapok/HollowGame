module Program
open Raylib_cs
open System.Numerics
open DeathSceneCreator
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

let spawnAttack (startPoint:Point) (targetPoint:Point) w h animations currentAnimationName id = 
    let color = newColor 255uy 255uy 255uy 255uy
    let dx = float (targetPoint.X -  startPoint.X)
    let dy = float (targetPoint.Y -  startPoint.Y)
    let len = (dx**2+dy**2)**0.5
    let rel = len/1500.0
    let vx = dx/rel
    let vy = dy/rel
    makeGameObject startPoint 0 w h animations color currentAnimationName id "attackParticle" Kinematic (v2 startPoint.X startPoint.Y) (v2 vx vy)  (v2 0.0 0.0) InAir [
        {
            Offset = v2 (float w /4.0) (float h/4.0)
            Size = v2 (float w /2.0) (float h/2.0)
            Kind = Trigger
            Response = Overlap
            Name = "attackParticle"
        }
    ]
    
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
let downloadWinScene() =
    let d = 
        [| "resources/win_scene.png";  |]
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

let makePlayerAttack character player gameObjects = 
    let isAttacking, point = character.IsAttacking 
    let playerPoint = getPointOfGraphicObject player.GraphicObject
    if not isAttacking then []
    else
        [spawnAttack (playerPoint) {point with X=playerPoint.X+point.X-750; Y=playerPoint.Y+point.Y-500} 100 100 (Map.add "default" AttackAnimation Map.empty) "default" (10000+ List.length gameObjects)]
let makePlayerJump  character player = 
    if character.IsJumping && not character.IsInAir then 
        {player with PhysicalObject = {player.PhysicalObject with speed={X = player.PhysicalObject.speed.X; Y = -1000.0}}}
    else player
let makePlayerMove player character gameObjects = 
    let player = makePlayerWalkRight character player |> makePlayerWalkLeft character |> makePlayerJump character
    let projectile = makePlayerAttack character player gameObjects
    let hasStoppedRunning = abs player.PhysicalObject.speed.X < 0.2 && getCurrentAnimation player.GraphicObject = "Run"
    let hasStartedRunning = abs player.PhysicalObject.speed.X > 0.2 && getCurrentAnimation player.GraphicObject = "Standing"
    
    if hasStoppedRunning then { player with GraphicObject = changeGameObjectAnimation player.GraphicObject "Standing" }, projectile
    else if hasStartedRunning then {player with GraphicObject = changeGameObjectAnimation player.GraphicObject "Run" }, projectile
    else player, projectile

// Обновление поведения всех врагов
let updateEnemies (enemies: GameObject list) (player: GameObject) dt =
    enemies
    |> List.map (fun enemy ->
        // Определяем тип врага (можно расширить для хранения типа в GameObject)
        let enemyType = 
            DefaultEnemy
        
        updateEnemy enemy player enemyType dt)

let collisionHandler gameState collisions = 
    List.fold (fun acc x -> 
        match x with
        | _ when x.AName="Player" && x.BName="Enemy" || x.AName="Enemy" && x.BName="Player" -> {acc with GameMode = SetUpDeathScene}
        | _ when x.AName="attackParticle" && x.BName="Enemy" || x.AName="Enemy" && x.BName="attackParticle" -> 

            let delAlist = List.removeAt (List.findIndex (fun y -> y.PhysicalObject.id=x.A) acc.GameObjects) acc.GameObjects
            let delBlist = List.removeAt (List.findIndex (fun y -> y.PhysicalObject.id=x.B) delAlist) delAlist
            printfn "%A" (delBlist |> List.filter (fun x -> not (x.PhysicalObject.name ="Wall")))
            {acc with GameObjects=delBlist}
        | _ -> acc) gameState collisions 
let doFrame gameState =
    // Получаем игрока
    let player = Option.get (List.tryFind (fun x -> x.PhysicalObject.name = "Player") gameState.GameObjects)
    
    // Разделяем врагов и другие объекты
    let enemies = gameState.GameObjects |> List.filter (fun x -> x.PhysicalObject.name.StartsWith("Enemy"))
    let otherObjects = gameState.GameObjects |> List.filter (fun x -> not (x.PhysicalObject.name.StartsWith("Enemy")))
    
    // Обновляем поведение врагов
    let updatedEnemies = updateEnemies enemies player (1.0 / float gameState.FpsCount)
    
    // Объединяем обновленные враги с другими объектами
    let allGameObjects = otherObjects @ updatedEnemies
    let gameState = {gameState with GameObjects = allGameObjects}
    
    // Применяем физику ко всем объектам
    let bodies, collisions = nextFrame allGameObjects (1.0/float gameState.FpsCount) gameState.Camera
    let gameState = collisionHandler {gameState with GameObjects = bodies} collisions
    let bodies = gameState.GameObjects
    // Обновляем игрока (существующий код)
    let player = Option.get (List.tryFind (fun x -> x.PhysicalObject.name = "Player") bodies)
    let character = {
        IsWalkingLeft = false
        IsWalkingRight = false
        IsJumping = false
        IsInAir = if player.PhysicalObject.state=InAir then true else false
        IsAttacking = (false, newPoint 0 0 0.0f)
    }
    let newCharacter = gameState.InputHandler.CollectEvents() |> handleEvents character
    let (newPlayer: GameObject, projectile) = makePlayerMove player newCharacter gameState.GameObjects
    let camera = followingCamera gameState.Camera player 0.0001f
    {gameState with GameObjects = bodies @ projectile |> List.map(fun x -> if x.PhysicalObject.name="Player" then newPlayer else x); Camera = camera}


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
    let baseGameObjects = LoadGameObjects (loadBaseAnimation ()) @ [downloadBackground ()]

    
    // Создаем врагов
    let enemies = createEnemiesOnMap ()
    
    { gameState with 
        GameObjects = baseGameObjects @ enemies
        GameMode = MainLoop 
    }
let setUpDeathScene gameState = 
    let buttons = createDeathScene ()
    {
        GameMode = DeathScene;
        FpsCount = gameState.FpsCount;
        Buttons = buttons
        Camera = newMovableDepthCamera 0 0 1500 1000 0.001f 0.001f 1000f 0.0f

        GameObjects = [downloadDeathScene ()]
        InputHandler = gameState.InputHandler
    }
let doDeathScene gameState = 
    let buttonsGraph = gameState.Buttons |> List.map (fun x -> DrawableObject x.Bounds)
    let background = gameState.GameObjects |> List.map (fun x -> x.GraphicObject)

    drawAllVisibleObjects (List.concat [buttonsGraph; background] |> List.toArray) gameState.Camera
    let states = gameState.InputHandler.CollectEvents() |> List.map(fun x -> handleTransition gameState.GameMode gameState.Buttons x) |> List.filter(fun x -> not (x = gameState.GameMode))
    let state = if states |> List.length = 1 then states |> List.head else gameState.GameMode

    { gameState with GameMode = state }

let setUpWinScene gameState = 
    let buttons = createDeathScene ()
    {
        GameMode = DeathScene;
        FpsCount = gameState.FpsCount;
        Buttons = buttons
        Camera = newMovableDepthCamera 0 0 1500 1000 0.001f 0.001f 1000f 0.0f

        GameObjects = [downloadDeathScene ()]
        InputHandler = gameState.InputHandler
    }
let doWinScene gameState = 
    let buttonsGraph = gameState.Buttons |> List.map (fun x -> DrawableObject x.Bounds)
    let background = gameState.GameObjects |> List.map (fun x -> x.GraphicObject)

    drawAllVisibleObjects (List.concat [buttonsGraph; background] |> List.toArray) gameState.Camera
    let states = gameState.InputHandler.CollectEvents() |> List.map(fun x -> handleTransition gameState.GameMode gameState.Buttons x) |> List.filter(fun x -> not (x = gameState.GameMode))
    let state = if states |> List.length = 1 then states |> List.head else gameState.GameMode

    { gameState with GameMode = state } 
let rec doGameLoop gameState =
    if toBool (Raylib.WindowShouldClose()) then
        ()
    else
        
        let newGameState =
            match gameState.GameMode with
            | Menu -> setUpMenu gameState
            | LoadMainLoop -> loadMainLoop gameState
            | MainLoop -> doFrame gameState
            | DeathScene -> doDeathScene gameState
            | SetUpDeathScene -> setUpDeathScene gameState
            | WinScene -> setUpWinScene gameState
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
    }
    
    doGameLoop gameState
    Raylib.CloseWindow()
    0