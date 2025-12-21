#load "PhysicsEngine.fs"
open PhysicsEngine

let cfg = defaultConfig
let dt = 1.0 / 60.0

// =========================
// ПЛАТФОРМЫ
// =========================

let platform =
    {
        id = 1
        bodyType = Static
        pos = v2 0.0 360.0
        speed = v2 0.0 0.0
        acc = v2 0.0 0.0
        state = InAir
        colliders =
            [
                {
                    Offset = v2 0.0 0.0
                    Size = v2 500.0 40.0
                    Kind = Solid
                    Response = Block
                    Name = "Ground"
                }
            ]
    }

let player =
    {
        id = 2
        bodyType = Dynamic
        pos = v2 100.0 100.0
        speed = v2 0.0 0.0
        acc = v2 0.0 0.0
        state = InAir
        colliders =
            [
                {
                    Offset = v2 0.0 0.0
                    Size = v2 32.0 64.0
                    Kind = Solid
                    Response = Block
                    Name = "Player"
                }
            ]
    }

let mutable bodies = [ platform; player ]

for frame in 0 .. 180 do

    bodies <-
        bodies
        |> List.map (fun b ->
            if b.id <> 2 then b
            else
                let mutable speed = b.speed

                // ─── ПЕРЕМЕЩЕНИЕ ВПРАВО ───
                if frame >= 10 && frame <= 90 then
                    speed <- { speed with X = 120.0 }
                else
                    speed <- { speed with X = 0.0 }

                // ─── ПРЫЖОК ───
                if frame = 40 && b.state = OnGround then
                    speed <- { speed with Y = -900.0 }

                { b with speed = speed }
        )

    let step = physicsStep defaultConfig dt bodies
    bodies <- step.Bodies

    let p = bodies |> List.find (fun b -> b.id = 2)

    printfn
        "Frame %03d | Pos(%.1f, %.1f) Speed(%.1f, %.1f) State=%A"
        frame p.pos.X p.pos.Y p.speed.X p.speed.Y p.state
