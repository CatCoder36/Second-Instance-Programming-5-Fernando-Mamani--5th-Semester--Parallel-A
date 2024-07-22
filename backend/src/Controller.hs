{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controller where

import Servant
import Servant.Multipart
import Network.Wai
import GHC.Generics (Generic)
import Routes (ImageApi, imageApi)
import Services.ImageService (convertRGB8ToGray8, rgbToGray, pixelToChar, convertGrayImageToAscii, processImage)
import Codec.Picture
import Codec.Picture.Types (promoteImage)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

-- Get information about the API
-- Defines a handler to get basic information about the API.
-- This handler simply returns a string with static information.
--
-- Returns: A string with information about the API
infoGetHandler :: Handler String
infoGetHandler = return "IMAGE API | Student: Fernando Mauricio Mamani Navarro"

-- Convert an image to ASCII art
-- This handler takes multipart data from memory, tries to process
-- the contained image and returns the resulting ASCII art. In case 
-- of a processing error, it returns a 500 error.
--
-- Parameters:
-- multipartData: The multipart data containing the image
--
-- Returns: The ASCII representation of the image as a string
imageToAsciiHandler :: MultipartData Mem -> Handler String
imageToAsciiHandler multipartData = either (throwError . serverError) return (processImage multipartData)
  where
    serverError err = err500 { errBody = C8.pack err }

-- Define the server for the image API
-- The server definition connects the API type definition to the handlers
-- for each endpoint. It specifies the functions to be called when a request
-- is made to each endpoint.
--
-- Returns: The server for the image API
server :: Server ImageApi
server = infoGetHandler
    :<|> imageToAsciiHandler

-- Define the application
-- The application definition serves the image API using the specified server.
app :: Application
app = serve imageApi server
