let ASSERT boolStatement msg =
    if boolStatement
        then ()
        else failwith msg
type LightState =
    LIGHTOFF
    | LIGHTON
type Light = 
    {
        LightID:int;
        LightState:LightState
    }
type LightCommand =
    TURNON
    | TURNOFF    
let updateLight light lightCommand =
    match lightCommand with
        | TURNON->{light with LightState=LIGHTON}
        | TURNOFF->{light with LightState=LIGHTOFF}
// now we have an updateTick, although it does nothing
// note that due to the language, I have to return something
let updateTick =
    let recordsProcessed = None;
    recordsProcessed

[<EntryPoint>]
let main argv = 
    let myLight = {LightID=10;LightState=LIGHTOFF;}
    ASSERT ( (updateLight myLight TURNON).LightID = 10) "On remembers id"
    ASSERT ( (updateLight myLight TURNON).LightState = LIGHTON) "On remembers state"
    ASSERT (updateTick.IsNone) "No Lights Controlled During Initialization"
    0 // return an integer exit code
