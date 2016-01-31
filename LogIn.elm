module LogIn where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Effects exposing (Effects)

import AuthService exposing (authorize, AuthKey)


type alias Model = 
  { username : String 
  , password : String
  , authKey : Maybe AuthKey
  , failed : Bool}

init : Model
init = Model "" "" Nothing False

type LogInField = Username | Password

type Action = Type LogInField String | LogIn | Valid (Maybe AuthKey) 

noFx : Model -> (Model, Effects Action)
noFx model = (model, Effects.none)

update : Action -> Model -> (Model, Effects Action)
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

    LogIn -> (model, authorize model.username model.password Valid)


onInput : Signal.Address Action -> (String-> Action) -> Attribute
onInput address destination =
  let 
    handler str = 
      Signal.message address (destination str)
  in 
    on "input" targetValue handler

view : Signal.Address Action -> Model -> Html
view address model = 
  div [] 
  [ input
        [ placeholder "Username"
        , value model.username
        , onInput address (Type Username)
        ]
        []
  , br [] []
  , input
        [ placeholder "Password"
        , value model.password
        , onInput address (Type Password)
        ]
        []
  , br [] []
  , button [onClick address LogIn] [text "LogIn"]
  , br [] [] 
  , if model.failed 
    then text "You failed to guess that the username and password are 'admin'"
    else text ""
  ]

getAuthKey : Model -> Maybe AuthKey
getAuthKey model = model.authKey

