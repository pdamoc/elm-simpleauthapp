import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)

import LogIn
import FirstPage
import AuthService exposing (AuthKey)

import Html exposing (..)

type alias Model = 
  { authKey : Maybe AuthKey 
  , logInData : LogIn.Model Action
  , data : FirstPage.Model }

type Action = Auth LogIn.Action | FirstPage FirstPage.Action | Logout

init : (Model, Effects Action)
init = 
  ({ authKey = Nothing
  , logInData = LogIn.init Auth Logout
  , data = FirstPage.init
  } , Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of 
    Auth logInAction -> 
      let
        (logInData', fx) = LogIn.update logInAction model.logInData
        authKey = LogIn.getAuthKey logInData'
        data' = FirstPage.updateAuthKey authKey model.data

        model' = 
          { model | logInData = logInData'
          , authKey=authKey
          , data = data' }
      in 
        (model', fx) 

    FirstPage pageAction -> 
      let
        (data', fx) = FirstPage.update pageAction model.data
        shouldLogout = FirstPage.shouldLogout data'
      in 
        if shouldLogout 
        then update Logout model
        else ({model | data = data'}, Effects.map FirstPage fx)

    Logout -> 
        ({model | authKey = Nothing, logInData = LogIn.init Auth Logout}, Effects.none)

view : Signal.Address Action -> Model -> Html 
view address model =
  div []
  [ LogIn.view (Signal.forwardTo address Auth) model.logInData
  , br [] [] 

  , FirstPage.view (Signal.forwardTo address FirstPage) model.data   
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