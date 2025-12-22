module GameObjectLoader

open Animation
open PhysicsEngine
open MainLoop

let makeGameObject physObj graphObj = 
    {
        GraphicObject = physObj
        PhysicalObject = graphObj
    }


let LoadGameObjects animationMap = 
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
    }
    let sprite2 = DrawableObject {
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
    }
    let platform: PhysicsBody = {
        id = 2
        name = "Ground"
        bodyType = Static
        pos = v2 100.0 360.0
        speed = v2 0.0 0.0
        acc = v2 0.0 0.0
        state = InAir
        colliders =
            [
                {
                    Offset = v2 0.0 0.0
                    Size = v2 100.0 100.0
                    Kind = Solid
                    Response = Block
                    Name = "Ground"
                }
            ]
    }
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
                    Size = v2 400.0 300.0
                    Kind = Solid
                    Response = Block
                    Name = "Player"
                }
            ]
    }
    let obj1 = makeGameObject sprite1 platform
    let obj2 = makeGameObject sprite2 body
    [obj1; obj2]