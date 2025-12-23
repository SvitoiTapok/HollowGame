module PhysicsEngine

// =========================
// БАЗОВЫЕ ТИПЫ
// =========================

type V2 = { X: float; Y: float }
type Time = float
let v2 x y = { X = x; Y = y }

// =========================
// ФИЗИЧЕСКИЕ СОСТОЯНИЯ
// =========================

type MotionState =
    | OnGround
    | InAir
    | OnWallLeft
    | OnWallRight

// =========================
// ТЕЛА
// =========================

type BodyType =
    | Static
    | Dynamic
    | Kinematic

// =========================
// КОЛЛАЙДЕРЫ
// =========================

type ColliderKind =
    | Solid
    | Hitbox
    | Trigger

type CollisionResponse =
    | Block
    | Overlap
    | Ignore

type RelativeRect =
    {
        Offset: V2
        Size: V2
        Kind: ColliderKind
        Response: CollisionResponse
        Name: string
    }

type Rect =
    {
        X: float
        Y: float
        W: float
        H: float
        Name: string
    }

// =========================
// ОБЪЕКТ ФИЗИКИ
// =========================

type PhysicsBody =
    {
        id: int
        name: string
        bodyType: BodyType
        pos: V2
        speed: V2
        acc: V2
        colliders: RelativeRect list
        state: MotionState
    }

// =========================
// НАСТРОЙКИ
// =========================

type PhysicsConfig =
    {
        Gravity: float
        CellSize: float
        AirDrag: float
        GroundFriction: float
    }

let defaultConfig () =
    {
        Gravity = 2500.0
        CellSize = 64.0
        AirDrag = 0.0001
        GroundFriction = 12.0
    }

// =========================
// RECT UTILS
// =========================

module RectUtils =

    let absolute (pos: V2) (r: RelativeRect) =
        {
            X = pos.X + r.Offset.X
            Y = pos.Y + r.Offset.Y
            W = r.Size.X
            H = r.Size.Y
            Name = r.Name
        }

    let intersects (a: Rect) (b: Rect) =
        a.X < b.X + b.W &&
        a.X + a.W > b.X &&
        a.Y < b.Y + b.H &&
        a.Y + a.H > b.Y

    let penetration (a: Rect) (b: Rect) =
        if not (intersects a b) then None
        else
            let dx =
                if a.X < b.X then (b.X - (a.X + a.W))
                else ((b.X + b.W) - a.X)

            let dy =
                if a.Y < b.Y then (b.Y - (a.Y + a.H))
                else ((b.Y + b.H) - a.Y)

            if abs dx < abs dy then Some (v2 dx 0.0)
            else Some (v2 0.0 dy)

// =========================
// ПРОСТРАНСТВЕННАЯ СЕТКА
// =========================

type SpatialGrid =
    {
        CellSize: float
        Cells: Map<int * int, PhysicsBody list>
    }

module Grid =

    let empty size =
        { CellSize = size; Cells = Map.empty }

    let cell size v = int (floor (v / size))

    let rectCells size (r: Rect) =
        let startX = cell size r.X
        let endX = cell size (r.X + r.W)
        let startY = cell size r.Y
        let endY = cell size (r.Y + r.H)
        [ for x in startX .. endX do
          for y in startY .. endY -> (x, y) ]

    let insert body (rect: Rect) grid =
        rectCells grid.CellSize rect
        |> List.fold (fun g c ->
            let list = Map.tryFind c g.Cells |> Option.defaultValue []
            { g with Cells = Map.add c (body :: list) g.Cells }
        ) grid

    let query (rect: Rect) grid =
        rectCells grid.CellSize rect
        |> List.collect (fun c ->
            Map.tryFind c grid.Cells |> Option.defaultValue []
        )
        |> List.distinct

// =========================
// ИНТЕГРАЦИЯ + ЗАМЕДЛЕНИЕ
// =========================

let applyDrag (cfg: PhysicsConfig) (dt: Time) (b: PhysicsBody) =
    if b.bodyType <> Dynamic then b
    else
        let k =
            match b.state with
            | OnGround -> cfg.GroundFriction
            | _ -> cfg.AirDrag

        let dragX = -b.speed.X * k
        let dragY =
            if b.state = InAir then -b.speed.Y * (k * 0.1)
            else 0.0

        {
            b with
                acc =
                    {
                        X = b.acc.X + dragX
                        Y = b.acc.Y + dragY
                    }
        }

let integrate (cfg: PhysicsConfig) (dt: Time) (b: PhysicsBody) =
    if b.bodyType = Static then b
    else
        let acc =
            match b.bodyType with
            | Dynamic -> { b.acc with Y = b.acc.Y + cfg.Gravity }
            | _ -> b.acc

        let speed =
            {
                X = b.speed.X + acc.X * dt
                Y = b.speed.Y + acc.Y * dt
            }

        let pos =
            {
                X = b.pos.X + speed.X * dt
                Y = b.pos.Y + speed.Y * dt
            }

        { b with pos = pos; speed = speed; acc = acc }

// =========================
// КОЛЛИЗИИ
// =========================

type Collision =
    {
        A: int
        B: int
        AName: string
        BName: string
        Normal: V2
        ColliderA: RelativeRect
        ColliderB: RelativeRect
    }

let detectAllCollisions (a: PhysicsBody) (b: PhysicsBody) =
    a.colliders
    |> List.collect (fun ca ->
        b.colliders
        |> List.choose (fun cb ->
            let ra = RectUtils.absolute a.pos ca
            let rb = RectUtils.absolute b.pos cb
            RectUtils.penetration ra rb
            |> Option.map (fun n ->
                {
                    A = a.id
                    B = b.id
                    AName = a.name
                    BName = b.name
                    Normal = n
                    ColliderA = ca
                    ColliderB = cb
                })
        )
    )

let detectCollisions (a: PhysicsBody) (b: PhysicsBody) =
    a.colliders
    |> List.collect (fun ca ->
        b.colliders
        |> List.choose (fun cb ->
            match ca.Response, cb.Response with
            | Ignore, _ | _, Ignore -> None
            | _ ->
                let ra = RectUtils.absolute a.pos ca
                let rb = RectUtils.absolute b.pos cb
                RectUtils.penetration ra rb
                |> Option.map (fun n ->
                    {
                        A = a.id
                        B = b.id
                        AName = a.name
                        BName = b.name
                        Normal = n
                        ColliderA = ca
                        ColliderB = cb
                    })
        )
    )

// =========================
// РЕЗОЛВ
// =========================

let resolve (body: PhysicsBody) (normal: V2) =
    if body.bodyType <> Dynamic then body
    else
        if normal.Y < 0.0 then
            { body with
                pos = { body.pos with Y = body.pos.Y + normal.Y }
                speed = { body.speed with Y = 0.0 }
                state = OnGround
            }
        elif normal.Y > 0.0 then
            { body with
                pos = { body.pos with Y = body.pos.Y + normal.Y }
                speed = { body.speed with Y = 0.0 }
                state = InAir
            }
        elif normal.X > 0.0 then
            { body with
                pos = { body.pos with X = body.pos.X + normal.X }
                speed = { body.speed with X = 0.0 }
                state = OnWallLeft
            }
        else
            { body with
                pos = { body.pos with X = body.pos.X + normal.X }
                speed = { body.speed with X = 0.0 }
                state = OnWallRight
            }

// =========================
// ТОЧНОЕ РЕШЕНИЕ КОЛЛИЗИЙ С ПОЛОМ
// =========================

let resolveGroundCollision (body: PhysicsBody) (collisionNormal: V2) (floorRect: Rect) =
    if body.bodyType <> Dynamic || collisionNormal.Y >= 0.0 then body
    else
        // Находим самый нижний коллайдер тела
        let lowestCollider =
            body.colliders
            |> List.minBy (fun c -> body.pos.Y + c.Offset.Y)
        
        // Вычисляем точную позицию для тела
        let bodyBottomY = body.pos.Y + lowestCollider.Offset.Y + lowestCollider.Size.Y
        let floorTopY = floorRect.Y
        
        // Вычисляем насколько тело проникло в пол
        let penetrationDepth = bodyBottomY - floorTopY
        
        // Корректируем позицию точно на границу пола
        let correctedY = body.pos.Y - penetrationDepth
        
        { body with
            pos = { body.pos with Y = correctedY }
            speed = { body.speed with Y = 0.0 }
            state = OnGround
        }

// =========================
// ШАГ ФИЗИКИ
// =========================

type StepResult =
    {
        Bodies: PhysicsBody list
        Collisions: Collision list
    }

let physicsStep (cfg: PhysicsConfig) (dt: Time) (bodies: PhysicsBody list) =
    let prepared =
        bodies
        |> List.map (fun b -> { b with acc = v2 0.0 0.0 })
        |> List.map (applyDrag cfg dt)

    let moved =
        prepared |> List.map (integrate cfg dt)

    let grid =
        moved
        |> List.fold (fun g b ->
            b.colliders
            |> List.fold (fun gg c ->
                Grid.insert b (RectUtils.absolute b.pos c) gg
            ) g
        ) (Grid.empty cfg.CellSize)

    let collisions =
        moved
        |> List.collect (fun b ->
            b.colliders
            |> List.collect (fun c ->
                Grid.query (RectUtils.absolute b.pos c) grid
                |> List.filter (fun o -> o.id > b.id)
                |> List.collect (detectCollisions b)
            )
        )

    let cleared =
        moved |> List.map (fun b -> 
            // Проверяем, находится ли тело уже на земле и не движется ли вверх
            // Это предотвращает "прыжки в воздухе"
            match b.state with
            | OnGround when b.speed.Y = 0.0 -> 
                // Оставляем OnGround, если тело уже на земле и не движется вверх
                b
            | _ -> 
                { b with state = InAir }
        )

    // Разделяем коллизии на вертикальные и горизонтальные
    let verticalCollisions, horizontalCollisions =
        collisions
        |> List.partition (fun col -> col.Normal.Y <> 0.0)

    // Сначала обрабатываем вертикальные коллизии (особенно с полом)
    let verticallyResolved =
        verticalCollisions
        |> List.fold (fun bs col ->
            bs |> List.map (fun b ->
                if b.id = col.A then
                    // Определяем, кто из тел - пол (статический)
                    let otherBody = 
                        cleared |> List.find (fun o -> o.id = col.B)
                    
                    if otherBody.bodyType = Static && col.Normal.Y < 0.0 then
                        // Это коллизия с полом
                        let floorRect = RectUtils.absolute otherBody.pos col.ColliderB
                        resolveGroundCollision b col.Normal floorRect
                    else
                        resolve b col.Normal
                elif b.id = col.B then
                    let otherBody = 
                        cleared |> List.find (fun o -> o.id = col.A)
                    
                    if otherBody.bodyType = Static && col.Normal.Y > 0.0 then
                        // Это коллизия с полом (для второго тела нормаль инвертирована)
                        let floorRect = RectUtils.absolute otherBody.pos col.ColliderA
                        let invertedNormal = { col.Normal with X = -col.Normal.X; Y = -col.Normal.Y }
                        resolveGroundCollision b invertedNormal floorRect
                    else
                        resolve b { col.Normal with X = -col.Normal.X; Y = -col.Normal.Y }
                else b)
        ) cleared

    // Затем обрабатываем горизонтальные коллизии
    let resolved =
        horizontalCollisions
        |> List.fold (fun bs col ->
            bs |> List.map (fun b ->
                if b.id = col.A then resolve b col.Normal
                elif b.id = col.B then resolve b { col.Normal with X = -col.Normal.X; Y = -col.Normal.Y }
                else b)
        ) verticallyResolved

    { 
        Bodies = resolved; 
        Collisions =  
        moved
            |> List.collect (fun b ->
                b.colliders
                |> List.collect (fun c ->
                    Grid.query (RectUtils.absolute b.pos c) grid
                    |> List.filter (fun o -> o.id > b.id)
                    |> List.collect (detectAllCollisions b)
                )
            )
    }

let nextPhysFrame dt bodies = physicsStep (defaultConfig ()) dt bodies