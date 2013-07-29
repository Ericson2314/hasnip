module Main where

import Control.Concurrent

-- importing to ensure compilation
import HaSnip.ClientGameMain
import HaSnip.Misc
import HaSnip.ServerList


main :: IO ()
main = do setNumCapabilities =<< getNumCapabilities -- max cores!
          HaSnip.ServerList.run
