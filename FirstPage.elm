module FirstPage where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)

import Effects exposing (Effects)

import AuthService exposing (AuthKey)
import MessagingService exposing (postMessage)

type alias Model = 
  { messages : List String 
  , msg : String 
  , failure: Maybe String }

type Action = Type String | Post | Posted (Result String String)

init : Model
init = Model [] "" Nothing

noFx : Model -> (Model, Effects Action)
noFx model = (model, Effects.none)

update : AuthKey -> Action -> Model -> (Model, Effects Action)
update authKey action model = 
  case action of
    Type str -> noFx {model| msg = str}
    Post -> (model, postMessage authKey model.msg Posted)
    Posted result -> 
      case result of 
        Ok msg -> noFx <| Model (msg::model.messages) "" Nothing
        Err msg -> noFx <| Model model.messages "" (Just msg)

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
        [ placeholder "Enter Message"
        , value model.msg
        , onInput address Type
        ]
        []
  , button [onClick address Post] [text "Post"] 
  , br [] [] 
  , ul [] (List.map (\msg -> li [] [text msg] ) model.messages)
  , br [] []
  , text <| Maybe.withDefault "" model.failure
  ]    

shouldLogout : Model -> Bool
shouldLogout model = 
  (Maybe.withDefault "" model.failure) == "AuthKey Expired"