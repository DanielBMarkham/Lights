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
//for more than one light, our old schedule is now called event. And a schedule is a list of events
type LightEvent =
    {
        Light:Light;
        LightCommand:LightCommand;
        CommandTime:CommandTime;
    }
type LightSchedule = LightEvent[]
// factor the event and time matching to clean things up
let eventAndTimeMatch event time =
    match event.CommandTime.Day with
        | EVERYDAY->
            if event.CommandTime.Minute = time.Minute then true else false
        |_ -> if (event.CommandTime.Day = time.Day) && (event.CommandTime.Minute = time.Minute) then true else false
let updateTick schedule light time =
    // once we've factored out the match, this just becomes a filter
    let everyEventHappeningNow = schedule |> List.filter(fun x->eventAndTimeMatch x time)
    // every event for this light for this tick gets ran. Last one wins
    // if there are no events, the initial light is returned (TESTS CAUGHT THIS)
    if everyEventHappeningNow.IsEmpty
        then light
        else
            let howALightChangesForOneTick = everyEventHappeningNow |> List.map(fun x->updateLight x.Light x.LightCommand)
            howALightChangesForOneTick |> List.rev |> List.head
[<EntryPoint>]
let main argv = 
    let myLight = {LightID=10;LightState=LIGHTOFF;}
    ASSERT ( (updateLight myLight TURNON).LightID = 10) "On remembers id"
    ASSERT ( (updateLight myLight TURNON).LightState = LIGHTON) "On remembers state"
    // now these tests take arrays
    let myLightSchedule = [{Light = myLight; LightCommand=TURNON; CommandTime={Day=EVERYDAY;Minute=1200;};}]
    ASSERT ((updateTick myLightSchedule myLight {Day=SUNDAY;Minute=1199;}).LightState=LIGHTOFF)  "No Lights Controlled at the wrong time"
    
    let myEverydaySchedule = [{Light=myLight; LightCommand=TURNON;CommandTime={Day=EVERYDAY; Minute=1200;};}]
    ASSERT ((updateTick myEverydaySchedule myLight {Day=SUNDAY;Minute=1200}).LightState=LIGHTON) "light turns on at the scheduled time for everyday"

    let myTestLightEvent1 = {Light = myLight; LightCommand=TURNON; CommandTime={Day=MONDAY;Minute=800;};}
    let myTestLightEvent2 = {Light = myLight; LightCommand=TURNOFF; CommandTime={Day=TUESDAY;Minute=800;};}
    let myTestMultiEventSchedule = [myTestLightEvent1; myTestLightEvent2]
    let myLightMondayAt800 = updateTick myTestMultiEventSchedule myLight {Day=MONDAY;Minute=800}
    ASSERT (myLightMondayAt800.LightState=LIGHTON) "multi-times for one light turns it on"
    // Need to send in the new light here, not the old one
    let myLightTuesdayAt800 = updateTick myTestMultiEventSchedule myLightMondayAt800 {Day=TUESDAY;Minute=800}
    ASSERT (myLightTuesdayAt800.LightState=LIGHTOFF) "multi-times for one light turns it off"
    0 // return an integer exit code
