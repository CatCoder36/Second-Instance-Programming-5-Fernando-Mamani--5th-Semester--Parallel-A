port module Main exposing (main)

-- This module contains the main logic of the application.

import Browser
import Html exposing (..)
import Styles exposing (..)
import Components exposing (..)
import Types exposing (..)
-- Ports for reading file data
port requestFileRead : String -> Cmd msg
port receiveFileDataUrl : (String -> msg) -> Sub msg

-- Ports for send data to ImageApi
port sendFile : String -> Cmd msg
port receiveAscii : (String -> msg) -> Sub msg
port sendScaleFactor : Int -> Cmd msg


init : Model
init =
    { selectedImage = Nothing 
    , asciiArt = Nothing 
    , scaleFactor = 32 }

-- This function is called when a message is sent to the application.
-- Analyzes the message and returns the new model and the command to be executed.
-- In case of the msg should be OpendFileDialog, the model is not modified and no
-- command is executed.
-- In case of the msg should be FileSelected fileId, the model is not modified and
-- two commands are executed:
-- requestFileRead that receives the fileId for get the value of the image selected 
-- and sendFile that receives 
-- the fileId for sending the file to the API.
-- In case of the msg should be ReceiveFileDataUrl dataUrl, the model is updated with 
-- the dataUrl and no command is executed.
-- In case of the msg should be SetAsciiArt asciiArt, the model is updated with the asciiArt
-- and no command is executed.
-- In case of the msg should be ValueChanged newValue, the model is updated with the newValue
-- this value represent the scale factor
-- and this update is sent to the API for update the ascii art.
-- 
-- Parameters:
-- msg : Msg -> The message that was sent to the application.
-- model : Model -> The current model of the application.
--
-- Returns:
-- (Model, Cmd Msg) -> The new model and the command to be executed.
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OpenFileDialog ->
            (model, Cmd.none)
        FileSelected fileId ->
            let
                getUrlData = requestFileRead fileId
                sendToApi = sendFile fileId 
            in
            (model, Cmd.batch [ getUrlData, sendToApi ])

        ReceiveFileDataUrl dataUrl ->
            ({ model | selectedImage = Just dataUrl }, Cmd.none)
        SetAsciiArt asciiArt ->
            ({ model | asciiArt = Just asciiArt }, Cmd.none)
        ValueChanged newValue ->
            let
                updatedScaleFactor = Maybe.withDefault model.scaleFactor (String.toInt newValue)
                sendScaleFactorCmd = sendScaleFactor updatedScaleFactor
            in
            ({ model | scaleFactor = updatedScaleFactor }, sendScaleFactorCmd)

-- This function is called when the view of the application is updated.
-- Returns the HTML that will be displayed in the application.
-- The view is composed of the headerComponent, principalTitleComponent, imageSelectionComponent,
--  fileInputComponent, rangeInputComponent, textExplainDisminution and asciiArtPreviewComponent.
--
-- Parameters:
-- model : Model -> The current model of the application.
--
-- Returns:
-- Html Msg -> The HTML that will be displayed in the application.
view : Model -> Html Msg
view model =
    div Styles.mainDiv
        [ headerComponent model
        , principalTitleComponent
        , div Styles.containerImageAndAsciiStyle [
            imageSelectionComponent model
            , fileInputComponent 
        ]
        , rangeInputComponent model
        , textExplainDisminution
        , asciiArtPreviewComponent model
        
        ]

-- This function is called when the application is started.
-- Subscribes to the ports that will be used in the application for reading 
-- file data and receiving ascii art.
--
-- Parameters:
-- () -> The initial message of the application.
--
-- Returns:
-- (Model, Cmd Msg) -> The initial model and the command to be executed.
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveFileDataUrl ReceiveFileDataUrl
        , receiveAscii SetAsciiArt
        ]

-- Main function of the application.
-- Initializes the application with the initial model and the commands to be executed.
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (init, Cmd.none)  
        , update = update
        , view = view
        , subscriptions = subscriptions
        }