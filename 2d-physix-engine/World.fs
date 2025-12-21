module World

open Types
open Input

type World = {
    X: float
    Velocity: float
}

let initial = { X = 100.0; Velocity = 0.0 }

/// Обновление мира на основе текущего состояния клавиатуры
let update (dt: float) (inputState: InputState) (w: World) : World =
    let vx =
        if Input.isPressed inputState Key.Left then -100.0
        elif Input.isPressed inputState Key.Right then 100.0
        else 0.0
    
    { X = w.X + vx * dt; Velocity = vx }
