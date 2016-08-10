let ASSERT boolStatement msg =
    if boolStatement
        then ()
        else failwith msg

type Light =
        LIGHTOFF
        | LIGHTON

let updateLight light =
    light

[<EntryPoint>]
let main argv = 
    ASSERT (updateLight LIGHTOFF = LIGHTOFF) "Lights are not changed at initialization"
    0 // return an integer exit code
