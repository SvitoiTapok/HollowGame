module MainLoop

open Animation
open PhysicsEngine

type GameObject =
    {
        PhysicalObject: PhysicsBody list
        GraphicObject: GraphicObject
    }

let nextFrame (gameObject: GameObject) dt camera = 
    drawGraphicObject gameObject.GraphicObject camera
    nextPhysFrame dt gameObject.PhysicalObject
