module Rectangle

open Raylib_cs
open System 

type Rectangle = { 
    X: single
    Y: single
    W: int
    H: int
    Color: Color 
}

let random = Random()

let randomColor() : Color =
    let colors = [|
        Color.Red; Color.Green; Color.Blue; Color.Yellow; 
        Color.Orange; Color.Purple; Color.Pink; Color.SkyBlue;
        Color.Lime; Color.Gold; Color.Violet; Color.Beige
    |]
    colors.[random.Next(colors.Length)]

let generateRandomRectangle (maxX: single) (maxY: single) (minSize: int) (maxSize: int) : Rectangle =
    {
        X = single (random.Next(0, int maxX))
        Y = single (random.Next(0, int maxY))
        W = random.Next(minSize, maxSize)
        H = random.Next(minSize, maxSize)
        Color = randomColor()
    }

let generateRectangles (count: int) (maxX: single) (maxY: single) (minSize: int) (maxSize: int) : Rectangle[] =
    List.toArray [ for i in 1..count do generateRandomRectangle maxX maxY minSize maxSize ]