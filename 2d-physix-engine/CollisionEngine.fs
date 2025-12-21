module CollisionEngine

open Types

// Пространственное разбиение
type SpatialGrid = {
    CellSize: float
    Width: float
    Height: float
    Grid: Map<int * int, (MovableObj * Rect) list> // храним объекты с их bounding box
}

// Результат проверки коллизий
type CollisionResult = {
    NewObj: MovableObj
    Collisions: (Collider * V2) list // коллайдер, нормаль
}

// Поведение при столкновении
type CollisionBehavior = 
    | PassThrough    // Пройти насквозь
    | StopBefore     // Остановиться перед
    | Bounce         // Отскочить
    | StickToTop     // Прилипнуть к верхнему краю (только для вертикали вниз)

// Вспомогательные функции для работы с прямоугольниками
module RectUtils =
    let toAbsolute (pos: Pos) (relative: RelativeRect) : Rect =
        {
            X = pos.X + relative.Relative.X
            Y = pos.Y + relative.Relative.Y
            W = relative.W
            H = relative.H
            Name = relative.Name
        }

    let getAbsoluteColliders (pos: Pos) (colliders: Collider[]) : Rect[] =
        colliders
        |> Array.choose (function
            | Entity r | Platform r | Attack r | Trigger r -> 
                Some (toAbsolute pos r))

    let getBoundingBox (rects: Rect[]) : Rect =
        if Array.isEmpty rects then 
            { X = 0.0; Y = 0.0; W = 0.0; H = 0.0; Name = "empty" }
        else
            let minX = rects |> Array.map (fun r -> r.X) |> Array.min
            let minY = rects |> Array.map (fun r -> r.Y) |> Array.min
            let maxX = rects |> Array.map (fun r -> r.X + r.W) |> Array.max
            let maxY = rects |> Array.map (fun r -> r.Y + r.H) |> Array.max
            { 
                X = minX; Y = minY; 
                W = maxX - minX; H = maxY - minY; 
                Name = "bounding_box" 
            }

    let intersects (rect1: Rect) (rect2: Rect) : bool =
        rect1.X < rect2.X + rect2.W &&
        rect1.X + rect1.W > rect2.X &&
        rect1.Y < rect2.Y + rect2.H &&
        rect1.Y + rect1.H > rect2.Y

    let getIntersectionDepth (rect1: Rect) (rect2: Rect) : V2 option =
        if not (intersects rect1 rect2) then None
        else
            let dx1 = rect2.X - (rect1.X + rect1.W)
            let dx2 = (rect2.X + rect2.W) - rect1.X
            let dy1 = rect2.Y - (rect1.Y + rect1.H)
            let dy2 = (rect2.Y + rect2.H) - rect1.Y

            let dx = if abs dx1 < abs dx2 then dx1 else dx2
            let dy = if abs dy1 < abs dy2 then dy1 else dy2

            if abs dx < abs dy then
                Some { X = dx; Y = 0.0 }
            else
                Some { X = 0.0; Y = dy }

// Пространственная сетка
module SpatialGrid =
    let create (cellSize: float) (width: float) (height: float) : SpatialGrid =
        {
            CellSize = cellSize
            Width = width
            Height = height
            Grid = Map.empty
        }

    let getCellIndices (rect: Rect) (cellSize: float) : (int * int) list =
        let minX = int (rect.X / cellSize)
        let maxX = int ((rect.X + rect.W) / cellSize)
        let minY = int (rect.Y / cellSize)
        let maxY = int ((rect.Y + rect.H) / cellSize)
        
        [for i in minX..maxX do
         for j in minY..maxY do
         yield (i, j)]

    let addMovableObject (grid: SpatialGrid) (obj: MovableObj) : SpatialGrid =
        let boundingBox = 
            obj.colliders 
            |> RectUtils.getAbsoluteColliders obj.pos
            |> RectUtils.getBoundingBox

        let cells = getCellIndices boundingBox grid.CellSize

        let newGrid = 
            cells
            |> List.fold (fun acc cell ->
                let existing = Map.tryFind cell acc |> Option.defaultValue []
                Map.add cell ((obj, boundingBox) :: existing) acc) grid.Grid

        { grid with Grid = newGrid }

    let addImmovableObject (grid: SpatialGrid) (obj: ImmovableObj) : SpatialGrid =
        let boundingBox = 
            obj.colliders 
            |> RectUtils.getAbsoluteColliders obj.pos
            |> RectUtils.getBoundingBox

        let cells = getCellIndices boundingBox grid.CellSize

        let newGrid = 
            cells
            |> List.fold (fun acc cell ->
                let existing = Map.tryFind cell acc |> Option.defaultValue []
                // Для неподвижных объектов храним только bounding box
                Map.add cell existing acc) grid.Grid

        { grid with Grid = newGrid }

    let getNearbyObjects (grid: SpatialGrid) (boundingBox: Rect) : (MovableObj * Rect) list =
        let cells = getCellIndices boundingBox grid.CellSize

        cells
        |> List.collect (fun cell ->
            Map.tryFind cell grid.Grid 
            |> Option.defaultValue [])
        |> List.distinct

// Движок коллизий
module CollisionEngine =
    let private createGroundSensor (obj: MovableObj) : Rect option =
        // Находим основной коллайдер Entity для создания датчика
        let mainCollider = 
            obj.colliders 
            |> Array.tryPick (function 
                | Entity r -> Some r 
                | _ -> None)

        match mainCollider with
        | Some collider ->
            let absolute = RectUtils.toAbsolute obj.pos collider
            // Создаем датчик под объектом на основе его размеров
            let sensorHeight = absolute.H * 0.1 // 10% высоты объекта
            let sensorWidth = absolute.W * 0.8 // 80% ширины объекта
            let offsetX = (absolute.W - sensorWidth) / 2.0
            
            Some {
                X = absolute.X + offsetX
                Y = absolute.Y + absolute.H
                W = sensorWidth
                H = sensorHeight
                Name = "ground_sensor"
            }
        | None -> None

    let private checkCollision (obj1: MovableObj) (obj2: MovableObj) : (Collider * V2) list =
        obj1.colliders
        |> Array.toList
        |> List.collect (fun collider1 ->
            let absolute1 = 
                match collider1 with
                | Entity r | Platform r | Attack r | Trigger r -> 
                    RectUtils.toAbsolute obj1.pos r
            
            obj2.colliders
            |> Array.toList
            |> List.choose (fun collider2 ->
                let absolute2 = 
                    match collider2 with
                    | Entity r | Platform r | Attack r | Trigger r -> 
                        RectUtils.toAbsolute obj2.pos r
                
                match RectUtils.getIntersectionDepth absolute1 absolute2 with
                | Some normal -> Some (collider2, normal)
                | None -> None
            )
        )

    let private checkCollisionWithImmovable (obj: MovableObj) (immovable: ImmovableObj) : (Collider * V2) list =
        obj.colliders
        |> Array.toList
        |> List.collect (fun objCollider ->
            let absoluteObj = 
                match objCollider with
                | Entity r | Platform r | Attack r | Trigger r -> 
                    RectUtils.toAbsolute obj.pos r
            
            immovable.colliders
            |> Array.toList
            |> List.choose (fun immovableCollider ->
                let absoluteImmovable = 
                    match immovableCollider with
                    | Entity r | Platform r | Attack r | Trigger r -> 
                        RectUtils.toAbsolute immovable.pos r
                
                match RectUtils.getIntersectionDepth absoluteObj absoluteImmovable with
                | Some normal -> Some (immovableCollider, normal)
                | None -> None
            )
        )

    let private resolveHorizontalCollision (obj: MovableObj) (collider: Collider) (normal: V2) (behavior: CollisionBehavior) : MovableObj =
        match behavior with
        | PassThrough -> obj
        | StopBefore ->
            { obj with 
                pos = { obj.pos with X = obj.pos.X + normal.X }
                speed = { obj.speed with X = 0.0 }
            }
        | Bounce ->
            { obj with 
                pos = { obj.pos with X = obj.pos.X + normal.X }
                speed = { obj.speed with X = -obj.speed.X * 0.5 }
            }
        | StickToTop -> obj // не применяется для горизонтали

    let private resolveVerticalCollision (obj: MovableObj) (collider: Collider) (normal: V2) (behavior: CollisionBehavior) (isGroundSensor: bool) : MovableObj =
        match behavior with
        | PassThrough -> obj
        | StopBefore ->
            { obj with 
                pos = { obj.pos with Y = obj.pos.Y + normal.Y }
                speed = { obj.speed with Y = 0.0 }
            }
        | Bounce ->
            { obj with 
                pos = { obj.pos with Y = obj.pos.Y + normal.Y }
                speed = { obj.speed with Y = -obj.speed.Y * 0.5 }
            }
        | StickToTop when normal.Y < 0.0 && isGroundSensor -> // Только при движении вниз через датчик
            { obj with 
                pos = { obj.pos with Y = obj.pos.Y + normal.Y }
                speed = { obj.speed with Y = 0.0 }
            }
        | _ -> obj

    let private checkGroundCollisions (obj: MovableObj) (nearbyMovable: MovableObj list) (nearbyImmovable: ImmovableObj list) : (Collider * V2) list =
        match createGroundSensor obj with
        | Some groundSensor ->
            let checkWithObject (colliders: Collider[]) (getPos: unit -> Pos) =
                colliders
                |> Array.toList
                |> List.choose (fun collider ->
                    let absoluteCollider = 
                        match collider with
                        | Entity r | Platform r | Attack r | Trigger r -> 
                            RectUtils.toAbsolute (getPos()) r
                    
                    if RectUtils.intersects groundSensor absoluteCollider then
                        let normal = { 
                            X = 0.0; 
                            Y = absoluteCollider.Y - (groundSensor.Y + groundSensor.H) 
                        }
                        Some (collider, normal)
                    else
                        None
                )

            let movableGroundCollisions =
                nearbyMovable
                |> List.collect (fun movable ->
                    checkWithObject movable.colliders (fun () -> movable.pos))

            let immovableGroundCollisions =
                nearbyImmovable
                |> List.collect (fun immovable ->
                    checkWithObject immovable.colliders (fun () -> immovable.pos))

            movableGroundCollisions @ immovableGroundCollisions
        | None -> []

    let processCollisions (grid: SpatialGrid) (movableObjects: MovableObj list) (immovableObjects: ImmovableObj list) 
                        (horizontalBehavior: CollisionBehavior) (verticalBehavior: CollisionBehavior) : (MovableObj list * SpatialGrid) =
        
        // Обновляем пространственную сетку
        let updatedGrid = 
            movableObjects
            |> List.fold (fun acc obj -> 
                SpatialGrid.addMovableObject acc obj) grid

        let processObject (obj: MovableObj) : MovableObj =
            // Получаем bounding box объекта
            let objBoundingBox = 
                obj.colliders 
                |> RectUtils.getAbsoluteColliders obj.pos
                |> RectUtils.getBoundingBox

            // Получаем ближайшие объекты для проверки
            let nearbyMovableWithBox = 
                SpatialGrid.getNearbyObjects updatedGrid objBoundingBox
                |> List.map fst
                |> List.filter (fun o -> o <> obj)
            
            let nearbyMovable = nearbyMovableWithBox

            let nearbyImmovable =
                immovableObjects
                |> List.filter (fun immovable ->
                    let immovableBoundingBox = 
                        immovable.colliders 
                        |> RectUtils.getAbsoluteColliders immovable.pos
                        |> RectUtils.getBoundingBox
                    RectUtils.intersects objBoundingBox immovableBoundingBox
                )
            
            // Проверяем обычные коллизии
            let movableCollisions =
                nearbyMovable
                |> List.collect (fun other -> checkCollision obj other)
            
            let immovableCollisions =
                nearbyImmovable
                |> List.collect (fun immovable -> checkCollisionWithImmovable obj immovable)
            
            let allCollisions = movableCollisions @ immovableCollisions
            
            // Проверяем коллизии с землей
            let groundCollisions = checkGroundCollisions obj nearbyMovable nearbyImmovable
            
            // Разрешаем горизонтальные коллизии
            let afterHorizontal = 
                allCollisions
                |> List.filter (fun (_, normal) -> abs normal.X > 0.0 && abs normal.X > abs normal.Y)
                |> List.fold (fun currentObj (collider, normal) ->
                    resolveHorizontalCollision currentObj collider normal horizontalBehavior
                ) obj
            
            // Разрешаем вертикальные коллизии (кроме земли)
            let afterVertical =
                allCollisions
                |> List.filter (fun (_, normal) -> abs normal.Y > 0.0 && abs normal.Y >= abs normal.X)
                |> List.fold (fun currentObj (collider, normal) ->
                    resolveVerticalCollision currentObj collider normal verticalBehavior false
                ) afterHorizontal
            
            // Разрешаем коллизии с землей (особая обработка)
            let finalObj =
                groundCollisions
                |> List.fold (fun currentObj (collider, normal) ->
                    resolveVerticalCollision currentObj collider normal StickToTop true
                ) afterVertical
            
            finalObj
        
        // Обрабатываем все объекты
        let processedObjects = movableObjects |> List.map processObject
        
        // Создаем новую сетку с обработанными объектами
        let finalGrid = 
            processedObjects
            |> List.fold (fun acc obj -> 
                SpatialGrid.addMovableObject acc obj) (SpatialGrid.create grid.CellSize grid.Width grid.Height)
        
        (processedObjects, finalGrid)

// Публичный интерфейс
let createCollisionEngine (cellSize: float) (worldWidth: float) (worldHeight: float) =
    SpatialGrid.create cellSize worldWidth worldHeight

let updateCollisions grid movableObjects immovableObjects horizontalBehavior verticalBehavior =
    CollisionEngine.processCollisions grid movableObjects immovableObjects horizontalBehavior verticalBehavior