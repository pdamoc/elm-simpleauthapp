module MessagingService where

import AuthService exposing (AuthKey)
import Task exposing (Task)
import Effects exposing (Effects)

postMessage : AuthKey -> String -> (Result String String -> action) -> Effects action
postMessage authKey msg toAction= 
  Task.succeed (Ok msg)
  |> Task.map toAction
  |> Effects.task   