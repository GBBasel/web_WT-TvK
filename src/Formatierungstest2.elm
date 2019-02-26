module Main exposing (..)

import Browser
import String exposing (fromChar, replace)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (..)
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
    | Shop
    | Gewonnen

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
    , hirnzellen : Int
    , iq : Int
    , fehlfunktion : String
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
    , hirnzellen = 0
    , iq = 0
    , fehlfunktion = ""
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
    Shop ->
        Home
    Gewonnen ->
        Home


type Msg
    = GeneratePattern (List Int)
    | Change String
    | Submit
    | ChangeScreen
    | ChangeScreenToPattern
    | ChangeScreenToRechnung
    | ChangeScreenToHome
    | ChangeScreenToShop
    | GenerateRechnung (List Int)
    | RechnungChange String
    | RechnungSubmit
    | Tick Time.Posix
    | IncreaseIQ 
    | Win
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        increaseDifficulty = if model.inputContent == (String.join "" <| List.map String.fromChar model.pattern)  then 1 else 0
        prüf1 = if model.inputContentRechnung == ( String.fromInt (sum(model.rechnung))) then 1 else 0
        prüf2 = if model.inputContentRechnung == ( String.fromInt (sum(model.rechnung))) && model.azr == 1 then InsertPattern else model.screen
        prüf3 = if model.inputContentRechnung == ( String.fromInt (sum(model.rechnung))) then 10 else model.countdown
        prüf4 = if model.inputContentRechnung == ( String.fromInt (sum(model.rechnung))) then (1 * model.difficulty) else 0
        counter = if model.countdown < 1 then Home else model.screen
        countdownscreen = if model.screen == Math then 1 else 0
        enoughornot = if model.hirnzellen > 9 && (model.difficulty - model.iq) > 1 then 1 else 0
        enoughornot2 = if model.hirnzellen > 9 && (model.difficulty - model.iq) > 1 then 100 else 0
        enoughornot3 = if model.hirnzellen > 99 then Gewonnen else Shop
        szn = if model.hirnzellen > 9 && (model.difficulty - model.iq) > 1 then "" else "Hier wird nicht beschissen!"
    in
    case msg of
        GeneratePattern newpattern ->
            ( { model | pattern = (List.map Char.fromCode newpattern) }, Cmd.none)
        Change newContent ->
            ( { model | inputContent = newContent }, Cmd.none)
        Submit ->
            ( { model | difficulty = model.difficulty + increaseDifficulty, screen = nextscreen model.screen, inputContent = "" }, Cmd.none )
        ChangeScreen ->
            ( { model | screen = nextscreen model.screen }, Cmd.none)
        GenerateRechnung newRechnung ->
            ( { model | rechnung = newRechnung }, Cmd.none)
        RechnungChange newContent ->
            ( { model | inputContentRechnung = newContent }, Cmd.none)
        RechnungSubmit ->
            ( { model | azr = model.azr - prüf1, screen = prüf2, countdown = prüf3, hirnzellen = model.hirnzellen + prüf4 ,inputContentRechnung = "" }, Random.generate GenerateRechnung (Random.list 2 ( Random.int -100 100 )))
        Tick time ->
            ( { model | countdown = model.countdown - countdownscreen, screen = counter}, Cmd.none )
        ChangeScreenToHome ->
            ( { model | screen = Home, fehlfunktion = "" }, Cmd.none)
        ChangeScreenToPattern ->
            ( { model | screen = nextscreen model.screen, countdown = 10 }, Random.generate GeneratePattern (Random.list (model.difficulty - model.iq) ( Random.int 33 125 )))
        ChangeScreenToRechnung ->
            ( { model | screen = nextscreen model.screen, azr = (model.difficulty - model.iq), countdown = 10 }, Random.generate GenerateRechnung (Random.list 2 ( Random.int -100 100 )))
        ChangeScreenToShop ->
            ( { model | screen = Shop, countdown = 10}, Cmd.none)
        IncreaseIQ ->
            ( { model | iq = model.iq + enoughornot, hirnzellen = model.hirnzellen - enoughornot2, fehlfunktion = szn}, Cmd.none)
        Win ->
            ( { model | screen = enoughornot3}, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick


-- VIEW

view : Model -> Html Msg
view model =
    let
        divstyle = [ style "font-size" "24px"
                   , style "font-family" "Showcard Gothic"
                   , style "text-align" "center"
                   ]
                   
         
        buttonstyle = [ style "font-family" "Showcard Gothic"
                      , style "font-size" "24px"
                      , style "background-color" "orange"
                      , style "border-radius" "10%"
                      , style "border" "2px solid red"
                      ]

    in
    case model.screen of
        Home ->
            div divstyle
                [ p [] []
                , h1 [style "font-family" "Showcard Gothic"] [text "littlebit-of-big-bamboozled-BOT-bootcamp"]
                , p [] []
                , img [src "Robot.png", width 300, height 300] [] --Source of Image: https://previews.123rf.com/images/lineartestpilot/lineartestpilot1502/lineartestpilot150207612/36521104-retro-comic-book-style-cartoon-robot.jpg
                , p [] []
                , button ([ onClick ChangeScreenToPattern ]++buttonstyle) [ text "Spiel starten" ]
                , p [] []
                , button ([ onClick ChangeScreenToShop ]++buttonstyle) [ text "Shop" ]
                , p [] []
                , text <| "aktuelle Schwierigkeit: " ++ String.fromInt model.difficulty
                , p [] []
                , text <| "Anzahl Hirnzellen:" ++ String.fromInt model.hirnzellen
                , p [] []
                , text <| "Dein IQ:" ++ String.fromInt model.iq
                ]
        Pattern ->
            div ( style "margin-top" "175px" :: divstyle)
                [ text "Merke dir das folgende Muster:"
                , p [style "font-color" "red"] []
                , text <| String.join "" <| List.map String.fromChar model.pattern
                ,p [] []
                , text <| "aktuelle Schwierigkeit: "++String.fromInt model.difficulty
                , p [] []
                , button ([ onClick ChangeScreenToRechnung ]++buttonstyle) [ text "Weiter zum rechnen" ]
                , p [] []
                , text <| "Anzahl Hirnzellen:" ++ String.fromInt model.hirnzellen
                , p [] []
                , text <| "Dein IQ:" ++ String.fromInt model.iq
                ]
        Math ->
            div ( style "margin-top" "175px" :: divstyle )
                [text "Löse die Rechnungen:"
                , p [] []
                , text <| replace "+-" "-"<| String.join "+" <| List.map String.fromInt model.rechnung
                , p [] []
                , text <| "Verbleibende Rechnungen:"++String.fromInt model.azr
                , p [] []
                , input [ placeholder "", value model.inputContentRechnung, onInput RechnungChange ] []
                , p [] []
                , button ([ onClick RechnungSubmit ]++buttonstyle) [ text "Bestätigen" ]
                , p [] []
                , text <| "Verbleibende Zeit: "++String.fromInt model.countdown
                , p [] []
                , text <| "Anzahl Hirnzellen:" ++ String.fromInt model.hirnzellen
                , p [] []
                , text <| "Dein IQ:" ++ String.fromInt model.iq
                ]
        InsertPattern ->
            div ( style "margin-top" "175px" :: divstyle )
                [text "Gib das Muster ein, welches du dir gemerkt hast"
                , p [] []
                , input [value model.inputContent, onInput Change ] []
                , p [] []
                , button ([ onClick Submit ]++buttonstyle) [ text "Bestätigen" ]
                , p [] []
                , text <| "aktuelle Schwierigkeit: "++String.fromInt model.difficulty
                , p [] []
                , text <| "Anzahl Hirnzellen:" ++ String.fromInt model.hirnzellen
                , p [] []
                , text <| "Dein IQ:" ++ String.fromInt model.iq
                ]
        Shop ->
            div (style "margin-top" "150px" :: divstyle )
            [ h1 [style "font-family" "Showcard Gothic"] [text "Shop"]
            , text "Kauf dir mit deinen Hirnzellen ein wenig IQ"
            , p [] []
            , button  ([ onClick IncreaseIQ ]++buttonstyle) [ text "IQ erhöhen für 10 Hirnzellen" ]
            , p [] []
            , text <| model.fehlfunktion
            , p [] []
            , button  ([ onClick Win ]++buttonstyle) [ text "Gewinne mit 100 Hirnzellen das Spiel" ]
            , p [] []
            , button  ([ onClick ChangeScreenToHome ]++buttonstyle) [ text "Zurück zum Starbildschirm" ]
            , p [] []
            , text <| "Anzahl Hirnzellen:" ++ String.fromInt model.hirnzellen
            , p [] []
            , text <| "Dein IQ:" ++ String.fromInt model.iq
            ]
        Gewonnen ->
            div (style "margin-top" "300px" :: divstyle) [text "Wow, du hast gewonnen!" ]
            

-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
