module Main exposing (..)

import Browser
import String exposing (fromChar, replace)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (Html, button, div, p, table, td, text, tr, input)
import Random exposing (int)
import List exposing (sum)
import Time exposing (..)
import Array exposing (..)

-- MODEL

type alias Model = 
    { difficulty : Int
    , rechnung : List Int
    , inputContentRechnung : String 
    , countdown : Int
    }

initialModel : Model
initialModel =
    { difficulty = 1
    , rechnung = []
    , inputContentRechnung = ""
    , countdown = 10 
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none )

-- UPDATE

type Msg
    = RechnungRoll
    | GenerateRechnung (List Int)
    | RechnungChange String
    | RechnungSubmit
    | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        increaseDifficulty = if model.inputContentRechnung == ( String.fromInt (sum(model.rechnung)))  then 1 else 0
    in
    case msg of
        RechnungRoll ->
            ( model, Random.generate GenerateRechnung (Random.list (model.difficulty + 1)( Random.int -100 100 )))
        GenerateRechnung newRechnung ->
            ( { model | rechnung = newRechnung }, Cmd.none)
        RechnungChange newContent ->
            ( { model | inputContentRechnung = newContent }, Cmd.none)
        RechnungSubmit ->
            ( { model | difficulty = model.difficulty + increaseDifficulty }, Cmd.none )
        Tick time ->
            ( { model | countdown = (model.countdown - 1) }, Cmd.none )
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ text "Merke dir das folgende Muster:"
        , p [] []
        , text <| replace "+-" "-"<| String.join "+" <| List.map String.fromInt model.rechnung
        , p [] []
        , button [ onClick RechnungRoll ] [ text "Roll" ]
        , input [ placeholder "Solution", value model.inputContentRechnung, onInput RechnungChange ] []
        , div [] [ text model.inputContentRechnung ]
        , button [ onClick RechnungSubmit ] [ text "Submit" ]
        , text <| String.fromInt model.difficulty
        , text <| String.fromInt model.countdown
        ]

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions}