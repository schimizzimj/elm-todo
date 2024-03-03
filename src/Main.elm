module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, type_)


-- MODEL

type alias Task =
    {
        description : String
    ,   completed : Bool
    ,   id : Int
    }

type alias Model =
    { tasks: List Task
    , taskInput: String
    , nextId: Int
    }

init : Model
init =
    { tasks = []
    , taskInput = ""
    , nextId = 0
    }

newTask : String -> Int -> Task
newTask description id =
    { description = description
    , completed = False
    , id = id
    }

-- UPDATE

type Msg
    = AddTask
    | UpdateTaskInput String
    | CompleteTask Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model | nextId = model.nextId + 1
            ,   tasks =
                    if String.isEmpty model.taskInput then
                        model.tasks
                    else
                        (newTask model.taskInput model.nextId) :: model.tasks
            ,   taskInput = ""
            }
        UpdateTaskInput newTaskDescription ->
            { model | taskInput = newTaskDescription }
        CompleteTask taskId ->
            { model | tasks = List.map (completeTask taskId) model.tasks }

completeTask : Int -> Task -> Task
completeTask taskId task =
    if task.id == taskId then
        { task | completed = True }
    else
        task

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Add a new task...", onInput UpdateTaskInput, value model.taskInput ] []
        , button [ onClick AddTask ] [ text "Add task" ]
        , div [] (List.map viewTask model.tasks)
        ]

viewTask : Task -> Html Msg
viewTask task =
    div [] [ text task.description
    , button [ onClick (CompleteTask task.id) ] [ text "Complete" ]
    , text (if task.completed then " (completed)" else "")
    ]

-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
