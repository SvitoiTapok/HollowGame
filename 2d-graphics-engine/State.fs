module State 

type Object = {
    GraphicObject: GraphicObject,
    PhisycalObject: PhisycalObject
}

type State = {
    Camera: Camera
    Objects: Map<string, Object>
    EventLoop: EventLoop
}
