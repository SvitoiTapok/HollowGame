module GameObjectLoader

open Animation
open PhysicsEngine
open MainLoop



let makeGameObjectSimple physObj graphObj = 
    {
        GraphicObject = physObj
        PhysicalObject = graphObj
    }


let makeGameObject point layer w h animations color currentAnimationName id name bodyType pos speed acc state coliders =
    {
        GraphicObject = DrawableObject {
            Point = point
            Layer = layer
            W = w
            H = h
            Animations = animations
            Color = color
            CurrentAnimationName = currentAnimationName
            MirroredByHorizontal = false
        }
        PhysicalObject = {
            id = id
            name = name
            bodyType = bodyType
            pos = pos
            speed = speed
            acc = acc
            state = state
            colliders = coliders
        }
    }


let makeWall (point: Point) w h animation id name=
    let color = newColor 255uy 255uy 255uy 255uy
    let pos = v2 point.X point.Y
    let coliders=   [
            {
                Offset = v2 0.0 0.0
                Size = v2 (float w) (float h)
                Kind = Solid
                Response = Block
                Name = "Wall"
            }
        ]
    makeGameObject point 0 w h (Map.add "default" animation Map.empty) color "default" id name Static pos (v2 0.0 0.0) (v2 0.0 0.0) InAir coliders
    

let makeGorisontalPlatform x y idOffset len platpormList= 
    let animation1: Animation = [| "resources/ground_floor.png"; |] |> fun frames -> loadAnimation frames 1
    let animation2: Animation = [| "resources/ground_utes.png"; |] |> fun frames -> loadAnimation frames 1
    let animation3: Animation = [| "resources/ground_utes_left.png"; |] |> fun frames -> loadAnimation frames 1
    let platformList = List.init (len-2) (fun i ->
        makeWall (newPoint (x + 64 * (i+1)) y 0.0f) 64 64 animation1 (i + 1 + idOffset) "Wall"
    )
    List.concat [platpormList; List.concat [[makeWall (newPoint (int x) (int y) 0.0f) 64 64 animation2 (idOffset) "Wall"]; platformList; [makeWall (newPoint (int x+(len-1)*64) (int y) 0.0f) 64 64 animation3 (idOffset+len-1) "Wall"]]]


let LoadGameObjects animationMap = 

    let playerRunAnim  = [| "resources/green_run_1.png"; "resources/green_run_2.png"|] |> fun frames -> loadAnimation frames 10
    let playerStandingAnim = [| "resources/green_standing.png"; |] |> fun frames -> loadAnimation frames 1
    let playerAnnimationMap = 
        Map.empty
        |> Map.add "Run" playerRunAnim
        |> Map.add "Standing" playerStandingAnim
    let color = newColor 255uy 255uy 255uy 255uy
    let sprite1 = DrawableObject {
        Point = {
            X = 0
            Y = 0
            Z = 0f
        }
        Layer = 0
        W = 400
        H = 300
        Animations = animationMap
        Color = color
        CurrentAnimationName = "Attack"
        MirroredByHorizontal = false
    }
    let sprite2 = DrawableObject {
        Point = {
            X = 0
            Y = 0
            Z = 0f
        }
        Layer = 0
        W = 96
        H = 96
        Animations = playerAnnimationMap
        Color = color
        CurrentAnimationName = "Run"
        MirroredByHorizontal = false
    }
    // let platform: PhysicsBody = {
    //     id = 3
    //     name = "Ground"
    //     bodyType = Static
    //     pos = v2 100.0 360.0
    //     speed = v2 0.0 0.0
    //     acc = v2 0.0 0.0
    //     state = InAir
    //     colliders =
    //         [
    //             {
    //                 Offset = v2 0.0 0.0
    //                 Size = v2 100.0 100.0
    //                 Kind = Solid
    //                 Response = Block
    //                 Name = "Ground"
    //             }
    //         ]
    // }
    let body = {
        id = 1
        name = "Player"
        bodyType = Dynamic
        pos = v2 0.0 0.0
        speed = v2 1.0 0.0
        acc = v2 1.0 0.0
        state = InAir
        colliders =
            [
                {
                    Offset = v2 0.0 0.0
                    Size = v2 64.0 96.0
                    Kind = Solid
                    Response = Block
                    Name = "Player"
                }
            ]
    }
    let animation: Animation = [| "resources/ground_floor.png"; |] |> fun frames -> loadAnimation frames 1
    printfn "Creating ground objects"
    let groundList = List.init 101 (fun i ->
        makeWall (newPoint (64 * i) (900-64) 0.0f) 64 64 animation (i + 10) "Wall"
    )  
    let animation: Animation = [| "resources/ground_wall.png"; |] |> fun frames -> loadAnimation frames 1
    let rightWallList = List.init 100 (fun i ->
        makeWall (newPoint 6400 (900 - 64 * (i+2)) 0.0f) 64 64 animation (i + 110) "Wall"
    )  
    let animation: Animation = [| "resources/ground_wall_ceil.png"; |] |> fun frames -> loadAnimation frames 1
    let ceilList = List.init 99 (fun i ->
        makeWall (newPoint (64+64 * i) (-5570) 0.0f) 64 64 animation (i + 210) "Wall"
    )  
    let animation: Animation = [| "resources/ground_wall_left.png"; |] |> fun frames -> loadAnimation frames 1
    let leftWall = List.init 100 (fun i ->
        makeWall (newPoint 0 (900 - 64 * (i+2)) 0.0f) 64 64 animation (i + 310) "Wall"
    ) 
    let animation: Animation = [| "resources/ground_floor.png"; |] |> fun frames -> loadAnimation frames 1
    let obj2 = makeGameObjectSimple sprite2 body
    let platformList =  List.concat [[obj2]; groundList; rightWallList; leftWall; ceilList;]
    let platformList = makeGorisontalPlatform 300 (900-64-200) 410 10 platformList
    let platformList = makeGorisontalPlatform 500 (900-64-500) 430 20 platformList
    let platformList = makeGorisontalPlatform 2500 (900-64-550) 450 20 platformList
    let platformList = makeGorisontalPlatform 3500 (900-64-850) 470 8 platformList
    let platformList = makeGorisontalPlatform 3000 (900-64-1100) 478 8 platformList
    platformList