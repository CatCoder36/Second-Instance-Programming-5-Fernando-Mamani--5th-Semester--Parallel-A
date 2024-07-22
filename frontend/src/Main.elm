module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text)

-- Modelo
type alias Model =
    { message : String }

-- Inicialización del modelo
init : Model
init =
    { message = "Fernando Mauricio Mamani Navarro 1-" }

-- Mensajes
type Msg
    = NoOp

-- Actualización del modelo
update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

-- Vista
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.message ]
        ]

-- Programa principal
main =
    Browser.sandbox { init = init, update = update, view = view }
