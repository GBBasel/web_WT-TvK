module Main exposing (main)

import Array exposing (Array, get, repeat, set)
import Browser 
import Html exposing (Html, button, div, p, table, td, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String exposing (fromChar)http://localhost:8000


type alias Model =
    { board: Array Sign
    , player : Int }


initialModel : Model
initialModel =
    --{ board = Array.fromList ['X',' ',' ',' ','O','X','O','O','X']
    { board = repeat 9 None
    , player = 1 }


type Msg
    = Increment
    | Decrement 
    | Play Int

type Sign 
    = X 
    | O 
    | None

signstring : Sign -> String
signstring s = 
    case s of
        X ->
            "X"
        O ->
            "O"
        None ->
            " "


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | player = model.player + 1 }

        Decrement ->
            { model | player = model.player - 1 }

        Play n -> 
            
            { model | board = set n (if model.player == 1 then X else O) model.board, player = -model.player}

drawCell n = td [border, onClick <| Play n] [getSign n]
drawRow n = tr [] (List.map drawCell (List.range (n) (n+2)))
drawTable = 


view : Model -> Html Msg
view model = 
    let 
        border = style "border" "1px solid black"
        -- cell n = text <| fromSign <| Maybe.withDefault ' ' <|get n model.board
        cell n = get n model.board |> Maybe.withDefault None |> signstring|> text
    in 
        div []
            [ text "Tic-Tac-Toe:"
            , p [] []
            , table [] 
                [ tr [] 
                    [ td [border, onClick <| Play 0] [cell 0]
                    , td [border, onClick <| Play 1] [cell 1]
                    , td [border, onClick <| Play 2] [cell 2]
                    ]
                , tr [] 
                    [ td [border, onClick <| Play 3] [cell 3]
                    , td [border, onClick <| Play 4] [cell 4]
                    , td [border, onClick <| Play 5] [cell 5]
                    ]
                , tr [] 
                    [ td [border, onClick <| Play 6] [cell 6]
                    , td [border, onClick <| Play 7] [cell 7]
                    , td [border, onClick <| Play 8] [cell 8]
                    ]
                ]
            , p [] []
            --, button [ onClick Increment ] [ text "+1" ]
            , div [] [ text "Player: ", text <| String.fromInt model.player ]
            --, button [ onClick Decrement ] [ text "-1" ]
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update}