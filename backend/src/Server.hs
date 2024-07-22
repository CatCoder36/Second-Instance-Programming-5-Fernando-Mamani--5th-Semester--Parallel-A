{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import Servant
import Servant.Multipart
import Network.Wai
import GHC.Generics (Generic)
import Api (MiAPI, miApi)

testGetHandler :: Handler String
testGetHandler = return "Hello, world!"

imageToAsciiHandler :: MultipartData Mem -> Handler String
imageToAsciiHandler _ = return "ASCII Art Here"

server :: Server MiAPI
server = testGetHandler
    :<|> imageToAsciiHandler

-- Create a WAI Application 
app :: Application
app = serve miApi server
