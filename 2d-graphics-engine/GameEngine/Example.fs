module Example

open Raylib_cs
open Rectangle
open Camera
open Graphic

let toBool (x: CBool) : bool = x = CBool.op_Implicit true

let clamp (v: single) (lo: single) (hi: single) =
    if v < lo then lo elif v > hi then hi else v

type Input =
    { Left: bool
      Right: bool
      Up: bool
      Down: bool }

let readInput () =
    { Left  = toBool (Raylib.IsKeyDown KeyboardKey.Left)  || toBool (Raylib.IsKeyDown KeyboardKey.A)
      Right = toBool (Raylib.IsKeyDown KeyboardKey.Right) || toBool (Raylib.IsKeyDown KeyboardKey.D)
      Up    = toBool (Raylib.IsKeyDown KeyboardKey.Up)    || toBool (Raylib.IsKeyDown KeyboardKey.W)
      Down  = toBool (Raylib.IsKeyDown KeyboardKey.Down)  || toBool (Raylib.IsKeyDown KeyboardKey.S) }

let update (dt: single) (input: Input) (rect: Rectangle) =
    let speed = 250.0f
    let vx = (if input.Right then speed else 0.0f) + (if input.Left then -speed else 0.0f)
    let vy = (if input.Down  then speed else 0.0f) + (if input.Up   then -speed else 0.0f)

    let x' = rect.X + vx * dt
    let y' = rect.Y + vy * dt

    { rect with
        X = x'
        Y = y' }


let rec loop (m: Rectangle) (camera: Camera) (rects: Rectangle[]) =
    if toBool (Raylib.WindowShouldClose()) || toBool (Raylib.IsKeyPressed KeyboardKey.Escape) then
        ()
    else
        let dt = Raylib.GetFrameTime()
        let input = readInput()
        let m' = update dt input m
        let newCamera = followingCamera camera m' (dt/(single 100))
        printfn "%A %A" newCamera m'
        drawRectangles (Array.append rects [|m'|]) newCamera
        loop m' newCamera rects
