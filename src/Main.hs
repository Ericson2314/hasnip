module Main where

import Control.Concurrent

import HaSnip.ServerList

import HaSnip.Types
import HaSnip.Protocol.Ben

main :: IO ()
main = do setNumCapabilities =<< getNumCapabilities -- max cores!
          HaSnip.ServerList.run
