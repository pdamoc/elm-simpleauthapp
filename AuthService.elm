module AuthService where

import Task exposing (Task, andThen)
import Effects exposing (Effects, Never)

type alias AuthKey = String

authorize : String -> String -> (Maybe AuthKey -> a) -> Effects a
authorize username password toAction=
  let 
    validation = 
      if (username == "admin") && (password == "admin") 
      then Just "AuthKey"
      else Nothing
    task = 
      (Task.succeed validation)
      |> Task.map toAction
  in 
    Effects.task task
