module LogIn where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Effects exposing (Effects)
import Task

import AuthService exposing (authorize, AuthKey)


type alias Model parentAction = 
  { username : String 
  , password : String
  , authKey : Maybe AuthKey
  , failed : Bool
  , toParent : (Action-> parentAction)
  , onLogout : parentAction 
  }

init : (Action -> parentAction) -> parentAction -> Model parentAction
init toParentAction parentAction = 
  Model "" "" Nothing False toParentAction parentAction

type LogInField = Username | Password

type Action = Type LogInField String | LogIn | Valid (Maybe AuthKey) | Logout

noFx : Model parentAction -> (Model parentAction, Effects parentAction)
noFx model = (model, Effects.none)

update : Action -> Model parentAction -> (Model parentAction, Effects parentAction)
update action model = 
  case action of 
    Type field str -> 
      case field of 
        Username -> noFx {model | username = str}
        Password -> noFx {model | password = str}

    Valid authKey ->  
      let 
        failed = 
          case authKey of
            Nothing -> True
            Just s -> False
      in 
        noFx {model | authKey=authKey, failed = failed}

    LogIn -> 
      let 
        fx = 
          authorize model.username model.password Valid
          |> Effects.map model.toParent
      in 
        (model, fx)

    Logout -> (model, Effects.task <| Task.succeed model.onLogout)


onInput : Signal.Address Action -> (String-> Action) -> Attribute
onInput address destination =
  let 
    handler str = 
      Signal.message address (destination str)
  in 
    on "input" targetValue handler

view : Signal.Address Action -> Model parentAction -> Html
view address model = 
  case model.authKey of 
    Nothing -> 
      div [] 
      [ input
            [ placeholder "Username"
            , value model.username
            , onInput address (Type Username)
            ]
            []
      , input
            [ placeholder "Password"
            , value model.password
            , onInput address (Type Password)
            ]
            []
      , button [onClick address LogIn] [text "LogIn"]
      , br [] [] 
      , if model.failed 
        then text "You failed to guess that the username and password are 'admin'"
        else text ""
      ]
    Just _ -> 
      div [] [button [onClick address Logout] [text "Logout"] ]

getAuthKey : Model parentAction -> Maybe AuthKey
getAuthKey model = model.authKey

