module HaSnip.ClientGameMain where

import Prelude hiding (init)

import qualified Data.ByteString       as SB
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as CB

import Data.Word

import Control.Concurrent

import Network.Socket(SockAddr(SockAddrInet, SockAddrInet6))
import Network.Socket.ByteString

import Network.ENet
import qualified Network.ENet.Bindings as B
import qualified Network.ENet.Host as Host
import qualified Network.ENet.Peer as Peer

-- localhost w/ default port is: SockAddrInet 32887 16777343

init :: SockAddr -> IO ()
init servAddr = runInBoundThread $ withENetDo $ do
  client <- Host.create
            Nothing -- create a client host
            1       -- only allow 1 outgoing connection
            1       -- allow only 1 channel to be used, 0
            0       -- unlimited bandwidth in
            0       -- unlimited bandwidth out

  Host.compressWithRangeCoder client

  host <- Host.connect
          client   -- us
          servAddr -- server address
          1        -- use 1 channel
          3        -- 3 is AoS 0.75 magic number

  let loop = do temp <- Host.service client 5000
                case temp of
                  (Just (B.Event t p chan dat pack)) -> case t of
                    B.None       -> print "got none"       >> loop
                    B.Connect    -> print "got connect"    >> loop
                    B.Disconnect -> print "got disconnect" >> return ()
                    B.Receive    -> print "got receive"    >> loop
                  Nothing      ->   print "no packet"    >>   loop

  loop

  return ()
