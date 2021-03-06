module HaSnip.ServerList (run) where

import Prelude hiding (concat)

import Control.Monad

import Data.List (foldl')
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types

import Data.Foldable (concat)

import Network.URI
import Network.HTTP

import HaSnip.ServerList.Parse

getBnS :: AoSVersion -> URI
getBnS gameVer = fromJust $ parseAbsoluteURI $ stub ++ show gameVer
  where stub = "http://services.buildandshoot.com/serverlist.json?version="

getListing :: AoSVersion -> IO [Server]
getListing ver = fmap (concat . decode') . getResponseBody =<<
                 (simpleHTTP $ mkRequest GET $ getBnS ver)

getAll :: IO [Server]
getAll = fmap concat $ mapM getListing [Ben [0,75], Ben [0,76], PowerThirst]

run :: IO ()
run = print =<< getAll
