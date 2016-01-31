module LogIn where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Effects exposing (Effects)

import AuthService exposing (authorize)


type alias Model = 
  { username : String 
  , password : String
  , auth : Bool
  , failed : Bool}

init : Model
init = Model "" "" False False

type LogInField = Username | Password

type Action = Type LogInField String | LogIn | Valid Bool 

noFx : Model -> (Model, Effects Action)
noFx model = (model, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of 
    Type field str -> 
      case field of 
        Username -> noFx {model | username = str}
        Password -> noFx {model | password = str}

    Valid valid ->  noFx {model | auth=valid, failed = not valid}

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

isValid : Model -> Bool
isValid model = model.auth

