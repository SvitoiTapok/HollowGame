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
    


let LoadGameObjects animationMap gameObjects = 
    let playerRunAnim  = [| "resources/green_run_1.png"; "resources/green_run_2.png"|] |> fun frames -> loadAnimation frames 10
    let playerStandingAnim = [| "resources/green_standing.png"; |] |> fun frames -> loadAnimation frames 1
    let playerAnnimationMap = 
        Map.empty
        |> Map.add "Run" playerRunAnim
        |> Map.add "Standing" playerStandingAnim
    let color = newColor 255uy 255uy 255uy 255uy
    // let sprite1 = DrawableObject {
    //     Point = {
    //         X = 0
    //         Y = 0
    //         Z = 0f
    //     }
    //     Layer = 0
    //     W = 400
    //     H = 300
    //     Animations = animationMap
    //     Color = color
    //     CurrentAnimationName = "Attack"
    // }
    let sprite2 = DrawableObject {
        Point = {
            X = 0
            Y = 0
            Z = 0f
        }
        Layer = 0
        W = 100
        H = 100
        Animations = playerAnnimationMap
        Color = color
        CurrentAnimationName = "Run"
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
                    Size = v2 100.0 100.0
                    Kind = Solid
                    Response = Block
                    Name = "Player"
                }
            ]
    }
    let animation: Animation = [| "resources/ground_floor.png"; |] |> fun frames -> loadAnimation frames 1
    printfn "Creating ground objects"
    let groundList = List.init 100 (fun i ->
        makeWall (newPoint (64 * i) (900-64) 0.0f) 64 64 animation (i + 10) "Wall"
    )  
    let animation: Animation = [| "resources/ground_wall.png"; |] |> fun frames -> loadAnimation frames 1
    let platform1List = List.init 10 (fun i ->
        makeWall (newPoint (300 + 64 * i) (900-64-200) 0.0f) 64 64 animation (i + 110) "Wall"
    )  
    
    printfn "%b" (isVisible groundList.[10].GraphicObject (newMovableDepthCamera 0 0 1500 1000 0.001f 0.001f 1000f 0.0f))
    //let obj1 = makeGameObjectSimple sprite1 platform
    let obj2 = makeGameObjectSimple sprite2 body
    //printfn "%A" (List.concat [gameObjects;[obj1; obj2]; groundList])
    List.concat [gameObjects; [obj2]; groundList; platform1List]