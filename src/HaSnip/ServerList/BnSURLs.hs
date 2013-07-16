module HaSnip.ServerList.BnSURLs where

import Data.List

import Network.URI

import HaSnip.ServerList

getBnS :: AoSVersion -> URI -- version number | 'powerthirst'
getBnS gameVer = a
  where (Just a) = parseURI $ "http://services.buildandshoot.com/serverlist.json?version="
                   ++ case gameVer of
                     PowerThirst -> "powerthirst"
                     (Ben ns)    -> foldl' (\a c -> shows c $ '.':a) [] ns
