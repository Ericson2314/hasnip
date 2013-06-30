module Main where

import Control.Concurrent

main :: IO ()
main = do setNumCapabilities =<< getNumCapabilities -- max cores!
          putStrLn "this clearly doesn't do anything yet"
