{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api(MiAPI, miApi) where

import Servant
import Servant.Multipart
import Data.Text (Text)
import GHC.Generics (Generic)


type MiAPI = "testGet" :> Get '[JSON] String
        :<|> "image-to-ascci" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] String


miApi :: Proxy MiAPI
miApi = Proxy