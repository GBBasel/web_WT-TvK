module Main exposing (..)

import Browser
import String exposing (fromChar)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (Html, button, div, p, table, td, text, tr, input)
import Random exposing (int)
import List exposing (sum)
import Time exposing (..)

-- MODEL

type alias Model = 
    { difficulty : Int
    , rechnung : List Int
    , inputContent : String 
    , countdown : Int }

initialModel : Model
initialModel =
    { difficulty = 1
    , rechnung = []
    , inputContent = ""
    , countdown = 10 }

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none )

-- UPDATE

type Msg
    = RRoll
    | GenerateRechnung (List Int)
    | RChange String
    | RSubmit
    | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        increaseDifficulty = if model.inputContent == ( String.fromInt (sum(model.rechnung)))  then 1 else 0
    in
    case msg of
        RRoll ->
            ( model, Random.generate GenerateRechnung (Random.list (model.difficulty + 1) ( Random.int 1 10 )))
        GenerateRechnung newRechnung ->
            ( { model | rechnung = newRechnung }, Cmd.none)
        RChange newContent ->
            ( { model | inputContent = newContent }, Cmd.none)
        RSubmit ->
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
        , text <| String.join "+" <| List.map String.fromInt model.rechnung
        , p [] []
        , button [ onClick RRoll ] [ text "Roll" ]
        , input [ placeholder "Solution", value model.inputContent, onInput RChange ] []
        , div [] [ text model.inputContent ]
        , button [ onClick RSubmit ] [ text "Submit" ]
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