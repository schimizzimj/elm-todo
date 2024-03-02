module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)

-- MODEL

type alias Model =
    Int

init : Model
init =
    0

-- UPDATE

type Msg
    = Increment
    | Decrement

update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (String.fromInt model)]
        , div [] [ button [onClick Increment ] [ text "+" ] ]
        , div [] [ button [onClick Decrement ] [ text "-" ] ]
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
