port module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, type_, class)
import Json.Decode as Decode
import Json.Encode as Encode
import Html.Attributes exposing (disabled)
import Html.Attributes exposing (checked)


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

init : Decode.Value -> (Model, Cmd Msg)
init flags =
    ( case Decode.decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            { tasks = []
            , taskInput = ""
            , nextId = 0
            }
    , Cmd.none)

newTask : String -> Int -> Bool -> Task
newTask description id completed =
    { description = description
    , completed = completed
    , id = id
    }

-- UPDATE

type Msg
    = AddTask
    | UpdateTaskInput String
    | CompleteTask Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddTask ->
            ({ model | nextId = model.nextId + 1
            ,   tasks =
                    if String.isEmpty model.taskInput then
                        model.tasks
                    else
                        (newTask model.taskInput model.nextId False) :: model.tasks
            ,   taskInput = ""
            }
            , Cmd.none)
        UpdateTaskInput newTaskDescription ->
            ({ model | taskInput = newTaskDescription }
            , Cmd.none)
        CompleteTask taskId ->
            ({ model | tasks = List.map (completeTask taskId) model.tasks }
            , Cmd.none)

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
        , div [] [ viewControls model ]
        ]

viewTask : Task -> Html Msg
viewTask task =
    div [] [ text task.description
    , input [ type_ "checkbox", onClick (CompleteTask task.id), disabled task.completed, checked task.completed ] []
    ]

viewControls : Model -> Html Msg
viewControls model =
    div [ class "task-controls" ] [ text (String.fromInt (List.length model.tasks) ++ " tasks " ++ "(" ++ String.fromInt (List.length (List.filter .completed model.tasks )) ++ " completed)") ]

-- MAIN

main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , view = view
        , subscriptions = \_ -> Sub.none
        }

-- PORTS

port setStorage : Encode.Value -> Cmd msg

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) = update msg oldModel
    in
    (newModel
    , Cmd.batch [ setStorage (encoder newModel), cmds ]
    )


-- DECODER/ENCODER

taskDecoder : Decode.Decoder Task
taskDecoder =
    Decode.map3 newTask
        (Decode.field "description" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "completed" Decode.bool)

decoder : Decode.Decoder Model
decoder =
    Decode.map2 (\tasks nextId -> { tasks = tasks, taskInput = "", nextId = nextId })
        (Decode.field "tasks" (Decode.list taskDecoder))
        (Decode.field "nextId" Decode.int)

taskEncoder : Task -> Encode.Value
taskEncoder task =
    Encode.object
        [ ( "description", Encode.string task.description )
        , ( "completed", Encode.bool task.completed )
        , ( "id", Encode.int task.id )
        ]

encoder : Model -> Encode.Value
encoder model =
    Encode.object
        [ ( "tasks", Encode.list taskEncoder model.tasks )
        , ( "nextId", Encode.int model.nextId )
        ]
