module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Browser.Dom
open Data

// MODEL

let estimations = ["cBi0_compensated";
                  "cBw0_compensated";
                  "cFw0_compensated";
                  "Retz_compensated";]

let maxFrames = 38
type Model = {
  currentSequence : int
  currentFrame : int
  currentEstimation : int
  isPlaying : bool
  slowdown : int
  ticksInFrame : int
  imsize : int
}

type Msg =
| NextFrame
| AutoNextFrame
| PreviousFrame
| NextEstimation
| PreviousEstimation
| IncreaseSlowdown
| DecreaseSlowdown
| TogglePaying
| NextSequence
| PreviousSequence
| IncreaseSize
| DecreaseSize


let imsizes = ["320px";"480px";"640px";"720px";"1080px"]

let init() : Model = {currentSequence = 0
                      currentFrame=0;
                      currentEstimation=3;
                      isPlaying=false
                      ticksInFrame=0;
                      slowdown = 1;
                      imsize = 0}

let increaseBoundedInt value bound = 
  if value = bound then
    0
  else
    value+1
let decreaseBoundedInt value bound = 
  if value = 0 then
    bound
  else
    value-1
// UPDATE
let timerTick dispatch =
    window.setInterval(fun _ ->
        dispatch AutoNextFrame
    , 42) |> ignore

let keyboardInput dispatch =
    document.addEventListener("keydown", (fun myEvent -> 
      let kbevent = myEvent:?>Browser.Types.KeyboardEvent
      if kbevent.key = "a" then dispatch PreviousFrame
      else if kbevent.key = "d" then dispatch NextFrame
      else if kbevent.key = "w" then dispatch NextEstimation
      else if kbevent.key = "s" then dispatch PreviousEstimation
      else if kbevent.key = "p" then dispatch TogglePaying
      else if kbevent.key = "z" then dispatch DecreaseSlowdown
      else if kbevent.key = "x" then dispatch IncreaseSlowdown
      else if kbevent.key = "q" then dispatch NextSequence
      else if kbevent.key = "e" then dispatch PreviousSequence
      else if kbevent.key = "n" then dispatch IncreaseSize
      else if kbevent.key = "m" then dispatch DecreaseSize
      ))

    
    // (fun keyboardEvent -> 
    //   keyboardEvent
    // )|> ignore


let update (msg:Msg) (model:Model) =
    match msg with
    | NextFrame -> {model with currentFrame = (increaseBoundedInt model.currentFrame (maxFrames-1)) } 
    | AutoNextFrame -> if model.isPlaying then
                        let tickupdatedModel = {model with ticksInFrame = model.ticksInFrame+1}
                        if tickupdatedModel.ticksInFrame >= model.slowdown then
                          let newframe = (increaseBoundedInt model.currentFrame (maxFrames-1))
                          {{model with currentFrame = newframe }  with ticksInFrame = 0}
                        else
                          tickupdatedModel
                       else
                        model 
    | PreviousFrame -> {model with currentFrame = (decreaseBoundedInt model.currentFrame (maxFrames-1))}
    | NextEstimation -> {model with currentEstimation = (increaseBoundedInt model.currentEstimation estimations.Length)}
    | PreviousEstimation -> {model with currentEstimation = (decreaseBoundedInt model.currentEstimation estimations.Length)}
    | TogglePaying -> {model with isPlaying = not model.isPlaying}
    | IncreaseSlowdown -> {model with slowdown = model.slowdown+1}
    | DecreaseSlowdown -> {model with slowdown = model.slowdown-1}
    | IncreaseSize -> {model with imsize = (increaseBoundedInt model.imsize (imsizes.Length-1))}
    | DecreaseSize -> {model with imsize = (decreaseBoundedInt model.imsize (imsizes.Length-1)) }

// VIEW (rendered with React)

let getImgSource (model:Model) framenumber= 
  let isSourceFrame = framenumber % 2 = 0
  let filename =  (framenumber.ToString()) + ".jpg"
  let postnamedir = 
    if isSourceFrame then
      "source_frames/" + filename
    else
      if model.currentEstimation = estimations.Length then
        "reference_frames/" + filename
      else
        estimations.[model.currentEstimation] + "/" + filename
  let imgsource = "sequences/" + sequences.[model.currentSequence] + "/" + postnamedir 
  let isVisible = model.currentFrame = framenumber
  img [Src imgsource; HTMLAttr.Height imsizes.[model.imsize]; Hidden (not isVisible) ]

let view (model:Model) dispatch =
  let images = Array.init 36 (getImgSource model) 
  let estimation = 
    if (model.currentEstimation = estimations.Length)  then
      "reference " + (model.currentFrame.ToString())
    else
       estimations.[model.currentEstimation]  + (model.currentFrame.ToString())
  div []
      [ h1[][str sequences.[model.currentSequence]]
        h2[][str estimation]
        h2[][str ("Slowdown : " + model.slowdown.ToString())]
        div[]images]
      

// App
Program.mkSimple init update view
|> Program.withSubscription (fun _ -> Cmd.ofSub timerTick)
|> Program.withSubscription (fun _ -> Cmd.ofSub keyboardInput)
//|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.run
