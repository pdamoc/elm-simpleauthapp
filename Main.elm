import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)

import LogIn
import FirstPage

import Html exposing (..)
import Html.Events exposing (onClick)

type alias Model = 
  { loggedIn : Bool 
  , logInData : LogIn.Model
  , data : FirstPage.Model }

type Action = Auth LogIn.Action | LoggedIn FirstPage.Action | Logout

init : (Model, Effects Action)
init = (Model False LogIn.init FirstPage.init , Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of 
    Auth logInAction -> 
      let
        (logInData', fx) = LogIn.update logInAction model.logInData
        loggedIn = LogIn.isValid logInData'
      in 
        ({model | logInData = logInData', loggedIn=loggedIn}, Effects.map Auth fx) 

    LoggedIn pageAction -> 
      let
        (data', fx) = FirstPage.update pageAction model.data
      in 
        ({model | data = data'}, Effects.map LoggedIn fx)

    Logout -> 
        ({model | loggedIn = False, logInData = LogIn.init }, Effects.none)

view : Signal.Address Action -> Model -> Html 
view address model =
  if model.loggedIn 
  then 
    div [] 
    [ FirstPage.view (Signal.forwardTo address LoggedIn) model.data   
    , br [] []
    , button [onClick address Logout] [text "Logout"]
    ]     
  else LogIn.view (Signal.forwardTo address Auth) model.logInData
  
app : StartApp.App Model
app = StartApp.start 
  { init = init
  , update = update
  , view = view
  , inputs = []}

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks