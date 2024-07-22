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
import Services.ImageService (convertRGB8ToGray8, rgbToGray, pixelToChar, convertGrayImageToAscii)
import Codec.Picture
import Codec.Picture.Types (promoteImage)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

testGetHandler :: Handler String
testGetHandler = return "IMAGE API | Student: Fernando Mauricio Mamani Navarro"


imageToAsciiHandler :: MultipartData Mem -> Handler String
imageToAsciiHandler multipartData = do 
    let fileList = files multipartData
    case fileList of 
        [file] -> do 
            let fileContent = fdPayload file
            case decodeImage (L.toStrict fileContent) of 
                Left err -> throwError $ err500 { errBody = C8.pack err }
                Right dynamicImage -> do
                    let grayImage = convertRGB8ToGray8 (convertRGB8 dynamicImage)
                    let asciiArt = convertGrayImageToAscii grayImage
                    return asciiArt

server :: Server ImageApi
server = testGetHandler
    :<|> imageToAsciiHandler


app :: Application
app = serve imageApi server
