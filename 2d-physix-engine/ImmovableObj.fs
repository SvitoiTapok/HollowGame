module ImmovableObj

open Types

let makeImmovableObj (objs: ImmovableObj list)  =
    let rec update (curr: ImmovableObj) (rest: ImmovableObj list) =
        if List.isEmpty rest then curr
        else
            let next = {
                sprites = Array.append curr.sprites rest.Head.sprites
                colliders = Array.append curr.colliders rest.Head.colliders  
                pos = curr.pos
            }
            update next rest.Tail
    
    if List.isEmpty objs then 
        { 
            sprites = [||]; 
            colliders = [||]; 
            pos = { X = 0.0; Y = 0.0 }
        }
    else
        update objs.Head objs.Tail

let makeMovableObj (objs: MovableObj list)  =
    let rec update (curr: MovableObj) (rest: MovableObj list) =
        if List.isEmpty rest then curr
        else
            let next = {
                sprites = Array.append curr.sprites rest.Head.sprites
                colliders = Array.append curr.colliders rest.Head.colliders  
                pos = curr.pos
                speed = curr.pos
            }
            update next rest.Tail
    
    if List.isEmpty objs then 
        { 
            sprites = [||]; 
            colliders = [||]; 
            pos = { X = 0.0; Y = 0.0 }
            speed = { X = 0.0; Y = 0.0 }
        }
    else
        update objs.Head objs.Tail

let applyAcceleration (obj: MovableObj) (accel : V2) (dt : Time)  =
    {
        sprites = obj.sprites;
        colliders = obj.colliders;
        pos = obj.pos
        speed = {X = accel.X * dt + obj.speed.X; Y = accel.Y * dt + obj.speed.Y}
    }

let moveObjNoCollision (obj: MovableObj) (dt : Time) = 
    [
    obj, 
    {
        sprites = obj.sprites;
        colliders = obj.colliders;
        pos = {X = obj.pos.X + obj.speed.X * dt; Y = obj.pos.Y + obj.speed.Y * dt}
        speed = obj.speed
    }
    ]
