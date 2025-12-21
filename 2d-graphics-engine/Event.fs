module Event

type EventState = 
| None
| Evoked
| Skipped

type Event = 
    Name: string
    State: EventState
    Condition: State -> bool
    Action: State  -> State 

type EventLoop = 
    Events: Event list


createEvent "hui" (fun x -> true) (fun x -> {state})
