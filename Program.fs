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
// OUCH! Now Light Events only need IDS, not entire lights. The abstraction failed
type LightEvent =
    {
        LightID:int;
        LightCommand:LightCommand;
        CommandTime:CommandTime;
    }
type LightSchedule = LightEvent[]
let eventAndTimeMatch event time =
    match event.CommandTime.Day with
        | EVERYDAY->
            if event.CommandTime.Minute = time.Minute then true else false
        |_ -> if (event.CommandTime.Day = time.Day) && (event.CommandTime.Minute = time.Minute) then true else false
// same code, only difference is either we return the initial light if no time match, or the processed light
// and we sort and process by light
let updateTick schedule lights time =
    let processEachLight schedule (light:Light) time = 
        let everyEventHappeningNow = schedule |> List.filter(fun x->eventAndTimeMatch x time)
        // Now that we're doing multiples (the 'M' in ZOMBIE -- need to check for empty lists. Normally this would have happened much earlier)
        if everyEventHappeningNow.IsEmpty
            then light
            else
                let everyEventHappeningNowForThisLight = everyEventHappeningNow |> List.filter(fun x->x.LightID=light.LightID)
                if everyEventHappeningNowForThisLight.IsEmpty
                then light
                else
                    let howALightChangesForOneTick = everyEventHappeningNowForThisLight |> List.map(fun x->updateLight light x.LightCommand)
                    howALightChangesForOneTick |> List.rev |> List.head
    lights |> List.map(fun x->processEachLight schedule x time)
[<EntryPoint>]
let main argv = 
    let myLight = {LightID=10;LightState=LIGHTOFF;}
    ASSERT ( (updateLight myLight TURNON).LightID = 10) "On remembers id"
    ASSERT ( (updateLight myLight TURNON).LightState = LIGHTON) "On remembers state"

    let myLightSchedule = [{LightID = 10; LightCommand=TURNON; CommandTime={Day=EVERYDAY;Minute=1200;};}]
    ASSERT ((updateTick myLightSchedule [myLight] {Day=SUNDAY;Minute=1199;}).Head.LightState=LIGHTOFF)  "No Lights Controlled at the wrong time"
    
    let myEverydaySchedule = [{LightID=10; LightCommand=TURNON;CommandTime={Day=EVERYDAY; Minute=1200;};}]
    ASSERT ((updateTick myEverydaySchedule [myLight] {Day=SUNDAY;Minute=1200}).Head.LightState=LIGHTON) "light turns on at the scheduled time for everyday"

    let myTestLightEvent1 = {LightID = 10; LightCommand=TURNON; CommandTime={Day=MONDAY;Minute=800;};}
    let myTestLightEvent2 = {LightID = 10; LightCommand=TURNOFF; CommandTime={Day=TUESDAY;Minute=800;};}
    let myTestMultiEventSchedule = [myTestLightEvent1; myTestLightEvent2]
    let myLightMondayAt800 = (updateTick myTestMultiEventSchedule [myLight] {Day=MONDAY;Minute=800}).Head
    ASSERT (myLightMondayAt800.LightState=LIGHTON) "multi-times for one light turns it on"

    let myLightTuesdayAt800 = (updateTick myTestMultiEventSchedule [myLightMondayAt800] {Day=TUESDAY;Minute=800}).Head
    ASSERT (myLightTuesdayAt800.LightState=LIGHTOFF) "multi-times for one light turns it off"

    let testLight1 = {LightID=42;LightState=LIGHTOFF;}
    let testLight2 = {LightID=43;LightState=LIGHTOFF;}
    let multiLights =[testLight1;testLight2]
    let myTestLightEvent1 = {LightID = 42; LightCommand=TURNON; CommandTime={Day=MONDAY;Minute=800;};}
    let myTestLightEvent2 = {LightID = 42; LightCommand=TURNOFF; CommandTime={Day=WEDNESDAY;Minute=800;};}
    let myTestLightEvent3 = {LightID = 43; LightCommand=TURNON; CommandTime={Day=TUESDAY;Minute=800;};}
    let myTestLightEvent4 = {LightID = 43; LightCommand=TURNOFF; CommandTime={Day=THURSDAY;Minute=800;};}
    let myMultiLightMultiTimeSchedule = [myTestLightEvent1; myTestLightEvent2; myTestLightEvent3; myTestLightEvent4]

    let LightsMondayAT799 = updateTick myMultiLightMultiTimeSchedule multiLights {Day=MONDAY; Minute=799}
    let LightsMondayAT800 = updateTick myMultiLightMultiTimeSchedule LightsMondayAT799 {Day=MONDAY; Minute=800}
    let LightsTuesdayAT799 = updateTick myMultiLightMultiTimeSchedule LightsMondayAT800 {Day=TUESDAY; Minute=799}
    let LightsTuesdayAT800 = updateTick myMultiLightMultiTimeSchedule LightsTuesdayAT799 {Day=TUESDAY; Minute=800}
    let LightsWednesdayAT799 = updateTick myMultiLightMultiTimeSchedule LightsTuesdayAT800 {Day=WEDNESDAY; Minute=799}
    let LightsWednesdayAT800 = updateTick myMultiLightMultiTimeSchedule LightsWednesdayAT799 {Day=WEDNESDAY; Minute=800}

    // helper function
    let checkLightFor (lights:Light list) lightID lightState =
        if lights |> List.exists(fun x->x.LightID=lightID)
            then
                let matchingLight = lights |> List.find(fun x->x.LightID=lightID)
                matchingLight.LightState = lightState
            else false

    ASSERT (checkLightFor LightsMondayAT799 42 LIGHTOFF) "multi-light multi-schedule init okay"
    ASSERT (checkLightFor LightsMondayAT800 42 LIGHTON) "multi-light multi-schedule turn first light on okay"
    ASSERT (checkLightFor LightsTuesdayAT799 42 LIGHTON) "multi-light multi-schedule doesn't forget"
    ASSERT (checkLightFor LightsTuesdayAT800 42 LIGHTON) "multi-light multi-schedule first light stays on"
    ASSERT (checkLightFor LightsTuesdayAT800 43 LIGHTON) "multi-light multi-schedule second light comes on"
    ASSERT (checkLightFor LightsWednesdayAT800 43 LIGHTON) "multi-light multi-schedule second light stays on"
    ASSERT (checkLightFor LightsWednesdayAT800 42 LIGHTOFF) "multi-light multi-schedule first light goes off"
    ASSERT (checkLightFor LightsWednesdayAT800 43 LIGHTON) "multi-light multi-schedule first light stays on after second light goes off"
    0 // return an integer exit code
