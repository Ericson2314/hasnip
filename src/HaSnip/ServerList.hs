{-# Language TemplateHaskell, OverloadedStrings #-}
module HaSnip.Serverlist where
 -- http://services.buildandshoot.com/serverlist.json?version=0.75

import Control.Applicative
import Control.Monad

import Data.String
import Data.Word
import qualified Data.Text as T

import Network (PortID)
import Network.Socket ( SockAddr(SockAddrInet)
                      , PortNumber(PortNum)
                      )

import System.Endian

import Data.Aeson
import Data.Aeson.TH

import HaSnip.Misc(convertSymbolName)


instance FromJSON SockAddr where
  parseJSON (String cs) =
    if "aos://" == T.take 5 cs
    then case T.split (==':') (T.drop 5 cs) of
      ns@(_:_:[]) -> pure $ SockAddrInet port ip
        where ip       = toBE32 $ fromLE32 $ read i -- don't forget endianness switch!
              port     = PortNum $ read p
              (i:p:[]) = map T.unpack ns
  -- failure
      _         -> mzero
    else           mzero
  parseJSON _   =  mzero

data AoSVersion = Ben [Int]
                | PowerThirst

instance FromJSON AoSVersion where
   parseJSON (String "PT") = pure PowerThirst
   parseJSON (String ver)  = pure $ Ben $ map (read . T.unpack) $ T.split (=='.') ver
   parseJSON _             = mzero -- failure

data Server = Server { s_name            :: String
                     , s_identifier      :: SockAddr
                     , s_map             :: String
                     , s_gameMode        :: String
                     , s_country         :: String
                     , s_latency         :: Int
                     , s_playersCurrent  :: Int
                     , s_playersMax      :: Int
                     , s_lastUpdated     :: Word32
                     , s_gameVersion     :: AoSVersion
                     }

$(deriveFromJSON (convertSymbolName . drop 2) ''Server)
