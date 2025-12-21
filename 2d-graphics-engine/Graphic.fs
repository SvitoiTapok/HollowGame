module Graphic

open Raylib_cs
open Camera
open Rectangle

let toBool (x: CBool) : bool = x = CBool.op_Implicit true

type Input = { 
    Left: bool
    Right: bool
    Up: bool
    Down: bool 
}

type GameState = { 
    Input: Input 
    Rectangles: Rectangle[]
}


let isVisible (rectangle: Rectangle) (camera: Camera) (inCameraX: int) (inCameraY: int) =
    0 <= inCameraX + rectangle.W && inCameraX - rectangle.W <= camera.W && 0 <= inCameraY + rectangle.H && inCameraY - rectangle.H <= camera.H
let drawRectangles (rectangles: Rectangle[]) (camera: Camera) =
    let drawOneRectangle (rectangle: Rectangle) =
        let inCameraX = int (rectangle.X - camera.X) + (camera.W / 2 - rectangle.W / 2)
        let inCameraY = int (rectangle.Y - camera.Y) + (camera.H / 2 - rectangle.H /2)
        if isVisible rectangle camera inCameraX inCameraY
            then Raylib.DrawRectangle (inCameraX, inCameraY, rectangle.W, rectangle.H, rectangle.Color)
    
    let rec loop (i: int) =
        if i >= rectangles.Length 
            then ()
        else 
            drawOneRectangle rectangles.[i]
            loop (i + 1)
    
    Raylib.BeginDrawing()
    Raylib.ClearBackground(Color(38uy, 46uy, 56uy, 255uy)) 
    loop 0
    Raylib.DrawText("ВОва лох", 10, 10, 20, Color.White)
    Raylib.EndDrawing()

