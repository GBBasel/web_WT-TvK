module Main exposing (..)

import Browser
import String exposing (fromChar)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (Html, button, div, p, table, td, text, tr, input)
import Random exposing (int)

-- MODEL

type Screen
    = Home
    | Pattern
    | Math
    | InsertPattern

type alias Model = 
    { difficulty : Int
    , pattern : List Char
    , inputContent : String
    , screen : Screen
    }

initialModel : Model
initialModel =
    { difficulty = 1
    , pattern = []
    , inputContent = ""
    , screen = Home
    }
    

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none )

-- UPDATE

type Msg
    = Roll
    | GeneratePattern (List Int)
    | Change String
    | Submit
    | ChangeScreen
    
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let 
        increaseDifficulty = if model.inputContent == (String.join "" <| List.map String.fromChar model.pattern)  then 1 else 0
    in
    case msg of
        Roll ->
            ( model, Random.generate GeneratePattern (Random.list model.difficulty ( Random.int 33 125 )))
        GeneratePattern newpattern ->
            ( { model | pattern = (List.map Char.fromCode newpattern) }, Cmd.none)
        Change newContent ->
            ( { model | inputContent = newContent }, Cmd.none)
        Submit ->
            ( { model | difficulty = model.difficulty + increaseDifficulty }, Cmd.none )
        ChangeScreen ->
            ( { model | screen = nextscreen model.screen }, Cmd.none)
        
        

      
      

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    case model.screen of
        Home ->
            div [][text "Home"
                , p [] []
                , button [ onClick ChangeScreen ] [ text "Spiel starten" ]
                ]
        Pattern ->
            div []
                [ text "Merke dir das folgende Muster:"
                , p [] []
                , text <| String.join "" <| List.map String.fromChar model.pattern
                , p [] []
                , button [ onClick Roll ] [ text "Roll" ]
                , text <| String.fromInt model.difficulty
                , p [] []
                , button [ onClick ChangeScreen ] [ text "Weiter zum rechnen" ]
                ]
        Math ->
            div [][text "Rechenspiel"
                , p [] []
                , button [ onClick ChangeScreen ] [ text "Weiter zum gemerkten Muster" ]
                ]
        InsertPattern ->
            div [][text "Gib das Muster ein, welches du dir gemerkt hast"
                , input [ placeholder "Text to reverse", value model.inputContent, onInput Change ] []
                , div [] [ text model.inputContent ]
                , button [ onClick Submit ] [ text "Submit" ]
                , text <| String.fromInt model.difficulty
                , p [] []
                , button [ onClick ChangeScreen ] [ text "Zurück zum Home" ]
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
