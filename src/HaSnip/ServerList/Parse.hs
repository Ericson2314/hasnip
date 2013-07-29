{-# Language TemplateHaskell, OverloadedStrings #-}
module HaSnip.ServerList.Parse where

import Control.Applicative
import Control.Monad

import Data.String
import Data.Word
import qualified Data.Text as T

import Network.Socket ( SockAddr(SockAddrInet) )

import Data.Aeson
import Data.Aeson.TH

import HaSnip.Misc(convertSymbolName)

(><) = T.append

instance FromJSON SockAddr where
  parseJSON (String (cs))
    | (pre, post) <- T.splitAt 6 cs
    , "aos://" == pre
    , [i,p] <- map T.unpack $ T.split (==':') post
    = pure $ SockAddrInet (toEnum $ read p) $ read i

    | otherwise  = mzero

instance ToJSON SockAddr where
  toJSON (SockAddrInet port ip) =
    String $ "aos://" >< (T.pack $ show ip) >< ":" >< (T.pack $ show port)

data AoSVersion = Ben [Int]
                | PowerThirst
                  deriving (Eq, Show, Read) -- show defined in BnSURL

instance FromJSON AoSVersion where
   parseJSON (String "PT") = pure PowerThirst
   parseJSON (String ver)  = pure $ Ben $ map (read . T.unpack) $ T.split (=='.') ver
   parseJSON _             = mzero -- failure

instance ToJSON AoSVersion where
  toJSON PowerThirst = String $ "PT"
  toJSON (Ben ns)   = String $ help ns
     where help (n:ns) = (T.pack $ show n) >< "." >< help ns

data Server = Server { s_name            :: String
                     , s_identifier      :: SockAddr
                     , s_map             :: String
                     , s_gameMode        :: String
                     , s_country         :: String
                     , s_latency         :: Int
                     , s_playersCurrent  :: Int
                     , s_playersMax      :: Int
                     , s_lastUpdated     :: Word64
                     , s_gameVersion     :: AoSVersion
                     , s_masterId        :: Int
                     }
            deriving (Eq, Show)

$(deriveFromJSON (convertSymbolName . drop 2) ''Server)
$(deriveToJSON (convertSymbolName . drop 2) ''Server)
