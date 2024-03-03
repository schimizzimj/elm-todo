module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, type_)


-- MODEL

type alias Model =
    { tasks: List String
    , newTask: String
    }

init : Model
init =
    { tasks = []
    , newTask = ""
    }

-- UPDATE

type Msg
    = AddTask
    | UpdateNewTask String

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model | tasks = model.newTask :: model.tasks, newTask = "" }
        UpdateNewTask newTask ->
            { model | newTask = newTask }


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Add a new task...", onInput UpdateNewTask, value model.newTask ] []
        , button [ onClick AddTask ] [ text "Add task" ]
        , div [] (List.map (\task -> div [] [ text task ]) model.tasks)
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
