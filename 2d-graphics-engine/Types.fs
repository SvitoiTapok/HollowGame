module Types

type V2 = { X: float; Y: float }

type Color = byte * byte * byte * byte

type Speed = V2

type Pos = V2

type Time = float

let v2 x y = { X = x; Y = y }

type Rect = { X: float; Y: float; W: float; H: float; Name: string}

type DrawableRect = {  X: float; Y: float; W: float; H: float; Name: string; Color: Color; Depth: float } 

type RelativeRect = { W: float; H: float; Name: string; Relative: V2}

type RelativeDrawableRect = { W: float; H: float; Name: string; Relative: V2; Color: Color; Depth: float }

type RectShape =
    | Absolute of Rect
    | Relative of RelativeRect
    | AbsoluteDrawable of DrawableRect
    | RelativeDrawable of RelativeDrawableRect

type Collider = 
    | Platform of RelativeRect
    | Entity of RelativeRect
    | Attack of RelativeRect
    | Trigger of RelativeRect

type ImmovableObj = {sprites: RelativeDrawableRect[]; colliders: Collider[] ; pos: Pos}

type MovableObj = {sprites: RelativeDrawableRect[]; colliders: Collider[] ; pos: Pos; speed: Speed }

let triggeToRectNames (trigger: Trigger) : string[] =
    [|"asd"|] //state ANY -> state FALL, state FALL -> state IDLE