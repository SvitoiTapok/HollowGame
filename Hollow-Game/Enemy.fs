// Enemy.fs
module Enemy

open PhysicsEngine
open Animation
open MainLoop
open GameObjectLoader

type EnemyType = {
    WalkSpeed: float
    JumpForce: float
    DetectionRange: float
    TexturePath: array<string> 
    Width: int
    Height: int
    ColliderSize: V2
}

let DefaultEnemy = {
    WalkSpeed = 400.0
    JumpForce = 1000.0
    DetectionRange = 500.0
    TexturePath = [|"resources/brown_run_1.png"; "resources/brown_run_2.png"|]
    Width = 96
    Height = 96
    ColliderSize = v2 64 96
}


let loadEnemyAnimation enemyType =
    let animation = 
        enemyType.TexturePath
        |> fun frames -> loadAnimation frames 10
    Map.add "Idle" animation Map.empty

let createEnemy (position: V2) (enemyType: EnemyType) (enemyAnimations: Map<string, Animation>) id=
    let point = { 
        X = int position.X
        Y = int position.Y
        Z = 0.0f
    }
    
    let color = newColor 255uy 255uy 255uy 255uy
    
    makeGameObject
        point
        4
        enemyType.Width
        enemyType.Height
        enemyAnimations
        color
        "Idle"
        id
        "Enemy"
        Dynamic
        position
        (v2 0.0 0.0)
        (v2 0.0 0.0)
        InAir
        [
            {
                Offset = v2 0.0 0.0
                Size = enemyType.ColliderSize
                Kind = Solid
                Response = Block
                Name = "EnemyCollider"
            }
        ]

let updateEnemy (enemy: GameObject) (player: GameObject) (enemyType: EnemyType) (dt: float) =
    let playerPos = player.PhysicalObject.pos
    let enemyPos = enemy.PhysicalObject.pos
    
    // Рассчитываем расстояние до игрока
    let distanceX = playerPos.X - enemyPos.X
    let distanceY = playerPos.Y - enemyPos.Y
    let distance = sqrt (distanceX * distanceX + distanceY * distanceY)
    
    // Если игрок в зоне обнаружения
    if distance <= enemyType.DetectionRange then
        // Определяем направление к игроку
        let direction = if distanceX > 0.0 then 1.0 else -1.0
        
        // Устанавливаем скорость движения в сторону игрока
        let newSpeedX = enemyType.WalkSpeed * direction
        
        let shouldJump = 
            enemy.PhysicalObject.state = OnGround && 
            distanceY < -50.0 &&
            abs distanceX < 200.0
        
        let newSpeedY = 
            if shouldJump then
                -enemyType.JumpForce
            else
                enemy.PhysicalObject.speed.Y
        
        { enemy with 
            PhysicalObject = { 
                enemy.PhysicalObject with 
                    speed = { X = newSpeedX; Y = newSpeedY }
            }
            GraphicObject = 
                if direction > 0.0 then
                    setMirroredByHorizontal enemy.GraphicObject true
                else
                    setMirroredByHorizontal enemy.GraphicObject false
        }
    else
        { enemy with 
            PhysicalObject = { 
                enemy.PhysicalObject with 
                    speed = { X = 0.0; Y = enemy.PhysicalObject.speed.Y }
            }
        }