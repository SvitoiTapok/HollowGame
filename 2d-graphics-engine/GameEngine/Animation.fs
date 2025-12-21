module Animation

open Raylib_cs
open System.Collections.Generic
open System.Numerics
open System.IO

type Point = { X: int; Y: int; Z: float32 }

let newPoint (x: int) (y: int) (z: float32) = { X = x; Y = y; Z = z }

let addX (point: Point) (x: int) = { point with X = point.X + x }
let addY (point: Point) (y: int) = { point with Y = point.Y + y }
let addZ (point: Point) (z: float32) = { point with Z = point.Z + z }
let setX (point: Point) (x: int) = { point with X = x }
let setY (point: Point) (y: int) = { point with Y = y }
let setZ (point: Point) (z: float32) = { point with Z = z }

let addPoint (point1: Point) (point2: Point) =
    { X = point1.X + point2.X
      Y = point1.Y + point2.Y
      Z = point1.Z + point2.Z }


type Camera =
    { X: int
      Y: int
      ZFar: single
      ZNear: single
      W: int
      H: int
      Speed: single
      initialSpeed: single }

let newDefaultCamera (x: int) (y: int) (w: int) (h: int) =
    { X = x; Y = y; W = w; H = h; ZFar = 1000.0f; ZNear = 0.0f; Speed = 0.0f; initialSpeed = 0.0f }

let newMovableCamera (x: int) (y: int) (w: int) (h: int) (speed: single) (initialSpeed: single) =
    { X = x; Y = y; W = w; H = h; ZFar = 1000.0f; ZNear = 0.0f; Speed = speed; initialSpeed = initialSpeed }

let newMovableDepthCamera (x: int) (y: int) (w: int) (h: int) (speed: single) (initialSpeed: single) (ZFar: single) (ZNear: single) =
    { X = x; Y = y; W = w; H = h; ZFar = ZFar; ZNear = ZNear; Speed = speed; initialSpeed = initialSpeed }

type Color =
    { R: byte
      G: byte
      B: byte
      Alpha: byte }

let newColor (r: byte) (g: byte) (b: byte) (a: byte) = { R = r; G = g; B = b; Alpha = a }

type Animation =
    { Name: string
      Frames: Texture2D[]
      CurrentFrame: int
      FrameCounter: int
      FrameSpeed: int }

type DrawableObject =
    { Point: Point
      Layer: int
      W: int
      H: int
      Color: Color
      Animations: Map<string, Animation>
      CurrentAnimationName: string }

 
type TextAlign =
    | Left
    | Center
    | Right

type TextObject =
    { Text: string
      Font: Font
      FontSize: int
      Spacing: single
      Color: Color
      Point: Point
      Level: int
      Align: TextAlign }

type GraphicObject =
    | DrawableObject of DrawableObject
    | TextObject of TextObject
    | RelativeGraphicObject of
        {| Object: GraphicObject
           RelatePoint: Point |}
    | MaskedGraphicObject of
        {| Object: GraphicObject
           Mask: Texture2D |}
    | Scope of
        {| Objects: GraphicObject[]
           Point: Point
           W: int
           H: int |}

let newDrawableObject (point: Point) (layer: int) (w: int) (h: int) (color: Color) (animations: Map<string, Animation>) (currentAnimationName: string) = 
    DrawableObject { Point = point; Layer = layer; 
        W = w; H = h; Color = color; 
        Animations = animations; CurrentAnimationName = currentAnimationName }

let newTextObject (text: string) (font: Font) (fontSize: int) (spacing: single) (color: Color) (point: Point) (level: int) (align: TextAlign) =
    TextObject { Text = text; Font = font; FontSize = fontSize; Spacing = spacing; Color = color; Point = point; Level = level; Align = align }

let newRelativeGraphicObject (object: GraphicObject) (relatePoint: Point) =
    RelativeGraphicObject {| Object = object; RelatePoint = relatePoint |}

let newMaskedGraphicObject (object: GraphicObject) (mask: Texture2D) =
    MaskedGraphicObject {| Object = object; Mask = mask |}

let newScope (objects: GraphicObject[]) (point: Point) (w: int) (h: int) =
    Scope {| Objects = objects; Point = point; W = w; H = h |}

let rec getLevel (gpaphicObject: GraphicObject) = 
    match gpaphicObject with
    | DrawableObject obj -> obj.Layer
    | TextObject text -> text.Level
    | RelativeGraphicObject obj -> getLevel obj.Object
    | MaskedGraphicObject masked -> getLevel masked.Object

let private toRaylibColor (color: Color) =
    Raylib_cs.Color(color.R, color.G, color.B, color.Alpha)

let getTextSize (textObj: TextObject) =
    Raylib.MeasureTextEx(
        textObj.Font,
        textObj.Text,
        float32 textObj.FontSize,
        textObj.Spacing)

let rec getSizesOfGraphicObject (graphicObject: GraphicObject) =
    match graphicObject with
    | DrawableObject obj -> obj.W, obj.H
    | TextObject text ->
        let size = getTextSize text
        int size.X, int size.Y
    | RelativeGraphicObject obj -> getSizesOfGraphicObject obj.Object
    | Scope scope -> scope.W, scope.H
    | MaskedGraphicObject masked -> getSizesOfGraphicObject masked.Object

let rec getPointOfGraphicObject (graphicObject: GraphicObject) =
    match graphicObject with
    | DrawableObject obj -> obj.Point
    | TextObject text -> text.Point
    | RelativeGraphicObject obj -> obj.RelatePoint
    | Scope scope -> scope.Point
    | MaskedGraphicObject masked -> getPointOfGraphicObject masked.Object

let rec addPointToGraphicObjectPoint (graphicObject: GraphicObject) (point: Point) =
    match graphicObject with
    | DrawableObject obj ->
        DrawableObject
            { obj with
                Point = addPoint obj.Point point }
    | TextObject text -> TextObject {text with Point = addPoint text.Point point }
    | RelativeGraphicObject obj ->
        RelativeGraphicObject
            {| obj with
                RelatePoint = addPoint obj.RelatePoint point |}

    | Scope scope ->
        Scope
            {| scope with
                Point = addPoint scope.Point point |}
    | MaskedGraphicObject masked ->
        MaskedGraphicObject
            {| masked with
                Object = addPointToGraphicObjectPoint masked.Object point |}

let isVisible (graphicObject: GraphicObject) (camera: Camera) =
    let { X = objectX; Y = objectY; Z = objectZ } = getPointOfGraphicObject graphicObject
    let objectWidth, objectHeigth = getSizesOfGraphicObject graphicObject
    let inCameraX = int (objectX - camera.X) + (camera.W / 2 - objectWidth / 2)
    let inCameraY = int (objectY - camera.Y) + (camera.H / 2 - objectHeigth / 2)

    camera.ZNear <= objectZ && objectZ <= camera.ZFar
    && 0 <= inCameraX + objectWidth
    && inCameraX - objectWidth <= camera.W
    && 0 <= inCameraY + objectHeigth
    && inCameraY - objectHeigth <= camera.H

let calculateParalaxFactor (graphicObject: GraphicObject) = 
    let { X = objectX; Y = objectY; Z = objectZ } = getPointOfGraphicObject graphicObject
    1.0f / (objectZ + 1.0f)

let worldToScreen (camera: Camera) (graphicObject: GraphicObject) (calculateScale: GraphicObject -> float32)=
    let p = getPointOfGraphicObject graphicObject
    let scale = calculateScale graphicObject

    let screenX =
        (float32 p.X - float32 camera.X) * scale
        

    let screenY =
        (float32 p.Y - float32 camera.Y) * scale
        

    { X = int screenX
      Y = int screenY
      Z = p.Z },
    scale

let drawTexture (texture: Texture2D) (width: int) (height: int) (point: Point) (color: Color) =
    let sourceRec = Rectangle(0.0f, 0.0f, float32 texture.Width, float32 texture.Height)

    let destRec =
        Rectangle(float32 point.X, float32 point.Y, float32 width, float32 height)

    Raylib.DrawTexturePro(texture, sourceRec, destRec, Vector2.Zero, 0.0f, toRaylibColor color)

let drawDrawableObject (object: DrawableObject) (camera: Camera) =
    let currentAnimation = object.Animations.[object.CurrentAnimationName]
    let currentTexture = currentAnimation.Frames.[currentAnimation.CurrentFrame]

    let screenPoint, scale = worldToScreen camera (DrawableObject object) calculateParalaxFactor

    let scaledW = int (float32 object.W * scale)
    let scaledH = int (float32 object.H * scale)
    drawTexture currentTexture scaledW scaledH screenPoint object.Color

let drawMask
    (masked:
        {| Object: GraphicObject
           Mask: Texture2D |})
    (camera: Camera)
    =
    let W, H = getSizesOfGraphicObject masked.Object

    let screenPoint, scale = worldToScreen camera (MaskedGraphicObject masked) calculateParalaxFactor

    let scaledW = int (float32 W * scale)
    let scaledH = int (float32 H * scale)

    let whiteColor =
        { R = 255uy
          G = 255uy
          B = 255uy
          Alpha = 255uy }

    drawTexture masked.Mask scaledW scaledH screenPoint whiteColor


let createMask (object: GraphicObject) (colorCalculator: Point -> Color) =
    let width, height = getSizesOfGraphicObject object
    let point = getPointOfGraphicObject object

    let mutable image = Raylib.GenImageColor(width, height, Color(0, 0, 0, 255))

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let color = toRaylibColor (colorCalculator { X = x; Y = y; Z = point.Z })
            Raylib.ImageDrawPixel(&image, x, y, color)

    Raylib.LoadTextureFromImage image

let loadAnimation (framePaths: string[]) frameSpeed =
    let frames = framePaths |> Array.map Raylib.LoadTexture

    { Name = "Attack"
      Frames = frames
      CurrentFrame = 0
      FrameCounter = 0
      FrameSpeed = frameSpeed }

let updateAnimation (animation: Animation) =
    if animation.Frames.Length > 1 then
        let newCounter = animation.FrameCounter + 1

        if newCounter >= animation.FrameSpeed then
            let nextFrame = (animation.CurrentFrame + 1) % animation.Frames.Length

            { animation with
                CurrentFrame = nextFrame
                FrameCounter = 0 }
        else
            { animation with
                FrameCounter = newCounter }
    else
        animation

let rec updateGraphicObjectAnimation (graphicObject: GraphicObject) =
    match graphicObject with
    | DrawableObject obj ->
        DrawableObject
            { obj with
                Animations =
                    obj.Animations
                    |> Map.change obj.CurrentAnimationName (Option.map updateAnimation) }
    | RelativeGraphicObject obj ->
        RelativeGraphicObject
            {| obj with
                Object = updateGraphicObjectAnimation obj.Object |}
    | MaskedGraphicObject obj ->
        MaskedGraphicObject
            {| obj with
                Object = updateGraphicObjectAnimation obj.Object |}
    | Scope scope ->
        Scope
            {| scope with
                Objects = scope.Objects |> Array.map updateGraphicObjectAnimation |}
    | _ -> graphicObject

let rec resetCurrentAnimation (graphicObject: GraphicObject) =
    match graphicObject with
    | DrawableObject obj ->
        DrawableObject
            { obj with
                Animations =
                    obj.Animations
                    |> Map.change obj.CurrentAnimationName (Option.map (fun animation ->
                        { animation with
                            CurrentFrame = 0
                            FrameCounter = 0 })) }
    | RelativeGraphicObject obj ->
        RelativeGraphicObject
            {| obj with
                Object = resetCurrentAnimation obj.Object |}
    | MaskedGraphicObject obj ->
        MaskedGraphicObject
            {| obj with
                Object = resetCurrentAnimation obj.Object |}
    | Scope scope ->
        Scope
            {| scope with
                Objects = scope.Objects |> Array.map resetCurrentAnimation |}
    | _ -> graphicObject

let rec addAnimationToGraphicAnimation (graphicObject: GraphicObject) (framePaths: string[]) (animationName: string) (frameSpeed: int) = 
    match graphicObject with
    | DrawableObject obj -> DrawableObject { obj with Animations = obj.Animations |> Map.add animationName (loadAnimation framePaths frameSpeed) }
    | RelativeGraphicObject obj -> RelativeGraphicObject {| obj with Object = addAnimationToGraphicAnimation obj.Object framePaths animationName frameSpeed |}
    | MaskedGraphicObject obj -> MaskedGraphicObject {| obj with Object = addAnimationToGraphicAnimation obj.Object framePaths animationName frameSpeed |}
    | Scope scope -> Scope {| scope with Objects = scope.Objects |> Array.map (fun obj -> addAnimationToGraphicAnimation obj framePaths animationName frameSpeed) |}
    | _ -> graphicObject

let rec changeGameObjectAnimation (graphicObject: GraphicObject) (animationName: string) =
    let object = resetCurrentAnimation graphicObject
    match object with
    | DrawableObject obj ->
        DrawableObject
            { obj with
                CurrentAnimationName = animationName }
    | RelativeGraphicObject obj ->
        RelativeGraphicObject
            {| obj with
                Object = changeGameObjectAnimation obj.Object animationName |}
    | MaskedGraphicObject obj ->
        MaskedGraphicObject
            {| obj with
                Object = changeGameObjectAnimation obj.Object animationName |}
    | Scope scope ->
        Scope
            {| scope with
                Objects =
                    scope.Objects
                    |> Array.map (fun obj -> changeGameObjectAnimation obj animationName) |}
    | _ -> object

let drawTextObject (textObj: TextObject) (camera: Camera) =
    let scale =
        1.0f

    let screenPoint, _ = worldToScreen camera (TextObject textObj) (fun _ -> 1.0f)

    let fontSize = float32 textObj.FontSize * scale

    let size =
        Raylib.MeasureTextEx(
            textObj.Font,
            textObj.Text,
            fontSize,
            textObj.Spacing)

    let x =
        match textObj.Align with
        | Left -> float32 screenPoint.X
        | Center -> float32 screenPoint.X + size.X / 2.0f
        | Right -> float32 screenPoint.X + size.X

    Raylib.DrawTextEx(
        textObj.Font,
        textObj.Text,
        Vector2(x, float32 screenPoint.Y),
        fontSize,
        textObj.Spacing,
        toRaylibColor textObj.Color)

let rec drawGraphicObject (graphicObject: GraphicObject) (camera: Camera) =
    let object = updateGraphicObjectAnimation graphicObject

    match object with
    | DrawableObject obj -> drawDrawableObject obj camera
    | RelativeGraphicObject obj -> drawGraphicObject (addPointToGraphicObjectPoint obj.Object obj.RelatePoint) camera
    | MaskedGraphicObject obj ->
        drawGraphicObject obj.Object camera
        drawMask obj camera
    | TextObject obj -> drawTextObject obj camera
    | Scope scope ->
        scope.Objects
        |> Array.map (fun obj -> addPointToGraphicObjectPoint obj scope.Point)
        |> Array.iter (fun obj -> drawGraphicObject obj camera)
    

let drawOnlyVisibleGraphicObject (graphicObject: GraphicObject) (camera: Camera) =
    if isVisible graphicObject camera then
        drawGraphicObject graphicObject camera 

let drawAll (objects: GraphicObject[]) (camera: Camera) =
    objects
    |> Array.sortBy (fun obj ->
        let point = getPointOfGraphicObject obj
        -(getLevel obj), -point.Z)
    |> Array.iter (fun obj -> drawGraphicObject obj camera)

let drawAllVisibleObjects (objects: GraphicObject[]) (camera: Camera) =
    objects
    |> Array.sortBy (fun obj ->
        let point = getPointOfGraphicObject obj
        -(getLevel obj), -point.Z)
    |> Array.iter (fun obj -> drawOnlyVisibleGraphicObject obj camera)


//TODO работа с текстом, batch + pipeline rendering, caching, render layer, Asset lifetime management
