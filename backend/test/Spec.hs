module Main (main) where

import Test.HUnit
import qualified ImageServiceTest as ImageTests

main :: IO ()
main = do
  putStrLn "Running Image Service Tests"
  imageResults <- runTestTT ImageTests.tests
  putStrLn "All tests completed"
  return ()