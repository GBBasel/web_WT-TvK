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

type Screen
    = Home
    | Pattern
    | Math
    | InsertPattern

type alias Model = 
    { difficulty : Int
    , dontcheat : Int
    , pattern : List Char
    , inputContent : String
    , screen : Screen
    , rechnung : List Int
    , inputContentRechnung : String 
    , countdown : Int
    , azr : Int
    }

initialModel : Model
initialModel =
    { difficulty = 1
    , dontcheat = 0
    , pattern = []
    , inputContent = ""
    , screen = Home
    , rechnung = []
    , inputContentRechnung = ""
    , countdown = 10 
    , azr = 0
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none )

-- UPDATE
    
nextscreen : Screen -> Screen
nextscreen oldscreen = case oldscreen of
    Home ->
        Pattern
    Pattern ->
        Math
    Math ->
        InsertPattern
    InsertPattern ->
        Home


type Msg
    = GeneratePattern (List Int)
    | Change String
    | Submit
    | ChangeScreen
    | ChangeScreenToPattern
    | ChangeScreenToRechnung
    | ChangeScreenToHome
    | GenerateRechnung (List Int)
    | RechnungChange String
    | RechnungSubmit
    | Tick Time.Posix
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        increaseDifficulty = if model.inputContent == (String.join "" <| List.map String.fromChar model.pattern)  then 1 else 0
        prüf1 = if model.inputContentRechnung == ( String.fromInt (sum(model.rechnung))) then 1 else 0
        prüf2 = if model.azr < 0 then Math else InsertPattern
        prüf3 = if model.inputContentRechnung == ( String.fromInt (sum(model.rechnung))) then 10 else model.countdown
        counter = if model.countdown < 1 then Home else model.screen
        countdownscreen = if model.screen == Math then 1 else 0
    in
    case msg of
        GeneratePattern newpattern ->
            ( { model | pattern = (List.map Char.fromCode newpattern) }, Cmd.none)
        Change newContent ->
            ( { model | inputContent = newContent }, Cmd.none)
        Submit ->
            ( { model | difficulty = model.difficulty + increaseDifficulty, screen = nextscreen model.screen }, Cmd.none )
        ChangeScreen ->
            ( { model | screen = nextscreen model.screen }, Cmd.none)
        GenerateRechnung newRechnung ->
            ( { model | rechnung = newRechnung }, Cmd.none)
        RechnungChange newContent ->
            ( { model | inputContentRechnung = newContent }, Cmd.none)
        RechnungSubmit ->
            ( { model | azr = model.azr - prüf1, screen = prüf2, countdown = prüf3 }, Random.generate GenerateRechnung (Random.list 2 ( Random.int -100 100 )))
        Tick time ->
            ( { model | countdown = model.countdown - countdownscreen, screen = counter}, Cmd.none )
        ChangeScreenToHome ->
            ( { model | screen = Home }, Cmd.none)
        ChangeScreenToPattern ->
            ( { model | screen = nextscreen model.screen, countdown = 10 }, Random.generate GeneratePattern (Random.list model.difficulty ( Random.int 33 125 )))
        ChangeScreenToRechnung ->
            ( { model | screen = nextscreen model.screen, azr = model.difficulty, countdown = 10 }, Random.generate GenerateRechnung (Random.list 2 ( Random.int -100 100 )))


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW

view : Model -> Html Msg
view model =
    let
        divstyle = [ style "position" "fixed"
                   , style "top" "50%"
                   , style "left" "50%"
                   , style "margin-top" "-100px"
                   , style "margin-left" "-200px"
                   ]
    in
    case model.screen of
        Home ->
            div divstyle [text "Home"
                , p [] []
                , button [ onClick ChangeScreenToPattern ] [ text "Spiel starten" ]
                ]
        Pattern ->
            div []
                [ text "Merke dir das folgende Muster:"
                , p [] []
                , text <| String.join "" <| List.map String.fromChar model.pattern
                ,p [] []
                , text <| "Schwierigkeit "++String.fromInt model.difficulty
                , p [] []
                , button [ onClick ChangeScreenToRechnung ] [ text "Weiter zum rechnen" ]
                ]
        Math ->
            div [][text "Rechenspiel"
                , p [] []
                , text <| replace "+-" "-"<| String.join "+" <| List.map String.fromInt model.rechnung
                , p [] []
                , text <| "Anzahl Rechnungen zu lösen:"++String.fromInt model.azr
                , p [] []
                , input [ placeholder "Solution", value model.inputContentRechnung, onInput RechnungChange ] []
                , div [] [ text model.inputContentRechnung ]
                , button [ onClick RechnungSubmit ] [ text "Submit" ]
                , text <| String.fromInt model.difficulty
                , text <| String.fromInt model.countdown
                ]
        InsertPattern ->
            div [][text "Gib das Muster ein, welches du dir gemerkt hast"
                , input [ placeholder "Text to reverse", value model.inputContent, onInput Change ] []
                , div [] [ text model.inputContent ]
                , button [ onClick Submit ] [ text "Submit" ]
                , text <| String.fromInt model.difficulty
                ]

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
