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
type Day = EVERYDAY | SUNDAY | MONDAY | TUESDAY | WEDNESDAY | THURSDAY | FRIDAY | SATURDAY
type CommandTime =
    {
        Day:Day;
        Minute:int;
    }
let updateLight light lightCommand =
    match lightCommand with
        | TURNON->{light with LightState=LIGHTON}
        | TURNOFF->{light with LightState=LIGHTOFF}
type LightSchedule =
    {
        Light:Light;
        LightCommand:LightCommand;
        CommandTime:CommandTime;
    }
let updateTick schedule light time =
    // I update the light if it is the right time.
    // Note that pattern matching forces me to exercise all the paths here
    let updatedLight =
        match schedule.CommandTime.Day with
            | EVERYDAY->
                if schedule.CommandTime.Minute = time.Minute then updateLight schedule.Light schedule.LightCommand else light
            |_ -> if (schedule.CommandTime.Day = time.Day) && (schedule.CommandTime.Minute = time.Minute) then updateLight schedule.Light schedule.LightCommand else light
    updatedLight

[<EntryPoint>]
let main argv = 
    let myLight = {LightID=10;LightState=LIGHTOFF;}
    let mySchedule = {Light = myLight; LightCommand=TURNON; CommandTime={Day=EVERYDAY;Minute=1200;};}
    // Note that because of immutability, myLight doesn't change. Each function returns a new light
    ASSERT ( (updateLight myLight TURNON).LightID = 10) "On remembers id"
    ASSERT ( (updateLight myLight TURNON).LightState = LIGHTON) "On remembers state"
    let myTestTime = {Day=SUNDAY;Minute=1199;}
    ASSERT ((updateTick mySchedule myLight myTestTime).LightState=LIGHTOFF)  "No Lights Controlled at the wrong time"
    0 // return an integer exit code
