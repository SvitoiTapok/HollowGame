module Camera

open Rectangle
let EPSILON = single 1e-2

type Camera = {
    X: single
    Y: single
    W: int 
    H: int
    Speed: single
    initialSpeed: single
}

let staticCamera camera =
    camera 

let followingCamera (camera: Camera) (rectangle: Rectangle) dSpeed = 
    let newX = camera.X - (camera.X - rectangle.X) * camera.Speed
    let newY = camera.Y - (camera.Y - rectangle.Y) * camera.Speed
    let isCame = abs (newX - rectangle.X) < EPSILON && abs(newY - rectangle.Y) < EPSILON
    let newCamera = { 
        camera with 
            X = newX
            Y = newY
            Speed = if isCame then camera.initialSpeed else camera.Speed + dSpeed
    }
    newCamera