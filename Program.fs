let ASSERT boolStatement msg =
    if boolStatement
        then ()
        else failwith msg

// Now lights have numbers
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

[<EntryPoint>]
let main argv = 
    // now the function has changed again and the tests are no longer valid
    //ASSERT (updateLight LIGHTOFF TURNON = LIGHTON) "Lights that are off can be turned on"
    //ASSERT (updateLight LIGHTON TURNOFF = LIGHTOFF) "Lights that are on can be turned off"
    let myLight = {LightID=10;LightState=LIGHTOFF;}
    ASSERT ( (updateLight myLight TURNON).LightID = 10) "On remembers id"
    ASSERT ( (updateLight myLight TURNON).LightState = LIGHTON) "On remembers state"
    0 // return an integer exit code
