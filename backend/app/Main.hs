module Main (main) where


import Network.Wai.Handler.Warp (run)
import Servant
import Api (MiAPI, miApi)

main :: IO ()
main = run 8080 (serve miApi servidor)

servidor :: Server MiAPI
servidor = return "Get response test"