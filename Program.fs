let ASSERT boolStatement msg =
    if boolStatement
        then ()
        else failwith msg

type Light =
    LIGHTOFF
    | LIGHTON

type LightCommand =
    TURNON
    | TURNOFF    

let updateLight light lightCommand =
    match lightCommand with
        | TURNON->LIGHTON
        | TURNOFF->LIGHTOFF

[<EntryPoint>]
let main argv = 
    // The function has changed. This test no longer valid
    // ASSERT (updateLight LIGHTOFF = LIGHTOFF) "Lights are not changed at initialization"
    ASSERT (updateLight LIGHTOFF TURNON = LIGHTON) "Lights that are off can be turned on"
    ASSERT (updateLight LIGHTON TURNOFF = LIGHTOFF) "Lights that are on can be turned off"

    0 // return an integer exit code
