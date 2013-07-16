module HaSnip.ClientGameMain where

import Prelude hiding (init)

import qualified Data.ByteString       as SB
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as CB

import Data.Word

import Network.Socket(SockAddr)
import Network.Socket.ByteString

import Network.ENet
import qualified Network.ENet.Bindings as B
import qualified Network.ENet.Host as Host
import qualified Network.ENet.Peer as Peer


init :: SockAddr -> IO ()
init servAddr = withENetDo $ do
  client <- Host.create
            Nothing         -- create a client host
            1               -- only allow 1 outgoing connection
            2               -- allow up 2 channels to be used, 0 and 1
            (57600 `div` 8) -- 56K modem with 56 Kbps downstream bandwidth
            (14400 `div` 8) -- 56K modem with 14 Kbps upstream bandwidth

  host <- Host.connect client servAddr 1 3

  let loop = do temp <- Host.service client 10000
                case temp of
                  Nothing -> return ()
                  (Just (B.Event t p chan dat pack)) -> case t of
                    B.None       -> print "no news"
                    B.Connect    -> print "incomming!!!!"
                    B.Disconnect -> print "payback!!!!"
                    B.Receive    -> print "news!!!!"
                loop
  loop

  return ()