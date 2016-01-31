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
  , failure: Maybe String 
  , authKey : Maybe AuthKey }

type Action = Type String | Post | Posted (Result String String)

init : Model
init = Model [] "" Nothing Nothing 

noFx : Model -> (Model, Effects Action)
noFx model = (model, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Type str -> noFx {model| msg = str}
    Post -> 
      case model.authKey of 
        Nothing -> noFx model
        Just authKey -> (model, postMessage authKey model.msg Posted)
    Posted result -> 
      case result of 
        Ok msg -> noFx <| Model (msg::model.messages) "" Nothing model.authKey
        Err msg -> noFx <| Model model.messages "" (Just msg) model.authKey

onInput : Signal.Address Action -> (String-> Action) -> Attribute
onInput address destination =
  let 
    handler str = 
      Signal.message address (destination str)
  in 
    on "input" targetValue handler


view : Signal.Address Action -> Model -> Html
view address model =
  case model.authKey of 
    Nothing -> text "You need to be logged in to post messages!"
    Just _ -> 
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

updateAuthKey : Maybe AuthKey -> Model -> Model
updateAuthKey key model = { model | authKey = key}