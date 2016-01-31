import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)

import LogIn
import FirstPage
import AuthService exposing (AuthKey)

import Html exposing (..)
import Html.Events exposing (onClick)

type alias Model = 
  { authKey : Maybe AuthKey 
  , logInData : LogIn.Model
  , data : FirstPage.Model }

type Action = Auth LogIn.Action | LoggedIn FirstPage.Action | Logout

init : (Model, Effects Action)
init = (Model Nothing LogIn.init FirstPage.init , Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of 
    Auth logInAction -> 
      let
        (logInData', fx) = LogIn.update logInAction model.logInData
        authKey = LogIn.getAuthKey logInData'
      in 
        ({model | logInData = logInData', authKey=authKey}, Effects.map Auth fx) 

    LoggedIn pageAction -> 
      case model.authKey of
        Nothing -> update Logout model
        Just authKey -> 
          let
            (data', fx) = FirstPage.update authKey pageAction model.data
          in 
            ({model | data = data'}, Effects.map LoggedIn fx)

    Logout -> 
        ({model | authKey = Nothing, logInData = LogIn.init }, Effects.none)

view : Signal.Address Action -> Model -> Html 
view address model =
  case model.authKey of
    Nothing -> 
      LogIn.view (Signal.forwardTo address Auth) model.logInData
    Just authKey -> 
      div [] 
      [ FirstPage.view (Signal.forwardTo address LoggedIn) model.data   
      , br [] []
      , button [onClick address Logout] [text "Logout"]
      ]       
  
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