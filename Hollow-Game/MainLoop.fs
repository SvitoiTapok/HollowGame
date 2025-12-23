module MainLoop

open Animation
open PhysicsEngine

type GameObject =
    {
        PhysicalObject: PhysicsBody
        GraphicObject: GraphicObject
    }

let V2ToPoint (v2: V2) (z: float32) : Point = 
    {X=(int v2.X); Y=(int v2.Y); Z=z}

let updateGraphPos (graphicObject: GraphicObject) (phisObj: PhysicsBody) =
    setPointToGraphicObjectPoint graphicObject (V2ToPoint phisObj.pos (getPointOfGraphicObject graphicObject).Z)


let nextFrame (gameObjects: GameObject list) dt camera = 
    let graphicObjects = gameObjects |> List.map (fun d -> d.GraphicObject) |> List.toArray |> updateAllObjectsAnimations
    drawAllVisibleObjects (graphicObjects) camera
    let physicsBodies = gameObjects |> List.map (fun d -> d.PhysicalObject)
    let updatedPhisics = (nextPhysFrame dt physicsBodies)
    let updatedPhysicsBodies = updatedPhisics.Bodies
    let updatedGameObjects =
        List.zip updatedPhysicsBodies (Array.toList graphicObjects)
        |> List.map (fun (newPhysicsObj, graphicObject) ->
            {
                PhysicalObject = newPhysicsObj
                GraphicObject = updateGraphPos graphicObject newPhysicsObj
            })
    updatedGameObjects, updatedPhisics.Collisions
