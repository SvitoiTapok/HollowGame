module Attack
open GameObjectLoader
open PhysicsEngine
open Animation

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
            Response = Ignore
            Name = "attackParticle"
        }
    ]
    