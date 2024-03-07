port module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, type_, class)
import Json.Decode as Decode
import Json.Encode as Encode
import Html.Attributes exposing (disabled)
import Html.Attributes exposing (checked)
import Html.Events exposing (keyCode)


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
    , showCompleted: Bool
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
            , showCompleted = True
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
    | ToggleCompleted
    | DeleteTask Int

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
        ToggleCompleted ->
            ({ model | showCompleted = not model.showCompleted }
            , Cmd.none)
        DeleteTask taskId ->
            ({ model | tasks = deleteTask taskId model.tasks }
            , Cmd.none)

completeTask : Int -> Task -> Task
completeTask taskId task =
    if task.id == taskId then
        { task | completed = True }
    else
        task

deleteTask : Int -> List Task -> List Task
deleteTask taskId tasks =
    List.filter (\task -> task.id /= taskId) tasks

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ viewInput model
        , div [] (List.map viewTask (if model.showCompleted then model.tasks else List.filter (not << .completed) model.tasks))
        , div [] [ viewControls model ]
        ]

viewInput : Model -> Html Msg
viewInput model =
    div []
        [ input 
            [ type_ "text"
            , placeholder "Add a new task..."
            , onInput UpdateTaskInput
            , onEnter AddTask
            , value model.taskInput
            ]
            []
        , button [ onClick AddTask ] [ text "Add task" ]
        ]

viewTask : Task -> Html Msg
viewTask task =
    div [
        class (if task.completed then "task completed" else "task")
    ] [ text task.description
    , input [ type_ "checkbox", onClick (CompleteTask task.id), disabled task.completed, checked task.completed ] []
    , button [ onClick (DeleteTask task.id) ] [ text "Delete" ]
    ]

viewControls : Model -> Html Msg
viewControls model =
    div [ class "task-controls" ] [ text (String.fromInt (List.length model.tasks) ++ " tasks " ++ "(" ++ String.fromInt (List.length (List.filter .completed model.tasks )) ++ " completed)")
        , button [ onClick ToggleCompleted ] [ text (if model.showCompleted then "Hide completed" else "Show completed") ]
        ]

-- Event handlers

onEnter : msg -> Html.Attribute msg
onEnter msg =
    Html.Events.on "keydown" (Decode.andThen (checkEnter msg) keyCode)

checkEnter : msg -> Int -> Decode.Decoder msg
checkEnter msg code =
    if code == 13 then
        Decode.succeed msg
    else
        Decode.fail "Not enter"

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
    Decode.map3 (\tasks nextId showCompleted -> { tasks = tasks, taskInput = "", nextId = nextId, showCompleted = showCompleted })
        (Decode.field "tasks" (Decode.list taskDecoder))
        (Decode.field "nextId" Decode.int)
        (Decode.field "showCompleted" Decode.bool)


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
        , ( "showCompleted", Encode.bool model.showCompleted)
        ]
