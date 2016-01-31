module AuthService where

import Task exposing (Task, andThen)
import Effects exposing (Effects, Never)

authorize : String -> String -> (Bool -> a) -> Effects a
authorize username password toAction=
  let 
    validation = (username == "admin") && (password == "admin")
    task = 
      (Task.succeed validation)
      |> Task.map toAction
  in 
    Effects.task task
