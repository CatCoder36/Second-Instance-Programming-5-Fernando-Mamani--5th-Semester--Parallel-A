{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Routes (ImageApi, imageApi) where

import Servant
import Servant.Multipart
import Data.Text (Text)
import GHC.Generics (Generic)

-- Image API type definition
-- The Image API type definition specifies the endpoints and methods of the image API.
-- The API consists of two endpoints: one for retrieving information about the API and one for converting an image to ASCII art.
-- The API uses the GET and POST methods to interact with the endpoints.
type ImageApi = "info" :> Get '[JSON] String
        :<|> "image-to-ascci" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] String

-- Proxy value for the Image API
-- Defines a proxy value for the image API, used to connect the API type definition to the web server.
-- This proxy facilitates operations such as routing and API documentation generation, allowing Servant to
-- to work with the API types at compile time.
--
-- Returns: A proxy value for the image API
imageApi :: Proxy ImageApi
imageApi = Proxy