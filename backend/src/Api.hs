{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant

type MiAPI = "mi-ruta" :> Get '[JSON] String

miApi :: Proxy MiAPI
miApi = Proxy