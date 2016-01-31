module FirstPage where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)

import Effects exposing (Effects)

type alias Model = 
  { messages : List String 
  , msg : String}

type Action = Type String | Post

init : Model
init = Model [] ""

noFx : Model -> (Model, Effects Action)
noFx model = (model, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    Type str -> noFx {model| msg = str}
    Post -> noFx <| Model (model.msg::model.messages) ""

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
  ]    
