{-# LANGUAGE LambdaCase #-}
module HaSnip.Protocol.Ben where

import Foreign.Ptr

import qualified Data.ByteString       as SB
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as CB

import Data.Word

import Control.Concurrent
import Control.Monad

import Network.Socket(SockAddr(SockAddrInet, SockAddrInet6))
import Network.Socket.ByteString

import Network.ENet
import qualified Network.ENet.Bindings as B
import qualified Network.ENet.Host     as Host
import qualified Network.ENet.Packet   as Packet
import qualified Network.ENet.Peer     as Peer

import Data.Word
import Data.Int
import Data.BitSet.Generic
import Data.ByteString

import HaSnip.Types

-- localhost w/ default port is: SockAddrInet 32887 16777343

data NetState = NS (Ptr B.Host) (Ptr B.Peer)

pre :: Word32 -> SockAddr -> IO NetState
pre magic servAddr = withENetDo $ do
  client <- Host.create
            Nothing -- create a client host
            1       -- only allow 1 outgoing connection
            1       -- allow only 1 channel to be used, 0
            0       -- unlimited bandwidth in
            0       -- unlimited bandwidth out

  -- ENet after the first couple versions uses this
  Host.compressWithRangeCoder client

  host <- Host.connect
          client   -- us
          servAddr -- server address
          1        -- use 1 channel
          magic    -- AoS-version-specific "magic number"

  return $ NS client host

post :: NetState -> IO ()
post (NS client peer) = Peer.disconnect peer 0 >> loop
  where loop :: IO ()
        loop = Host.service client 1000 >>= \case
          (Just (B.Event t _  _  _ p)) -> case t of
            B.Disconnect -> Peer.reset(peer)
            B.Receive    -> Packet.destroy p >> loop
            _            -> loop
          Nothing -> loop

receive :: world -> map -> Ptr B.Host -> NetState -> GState -> IO GState
receive w m client ns gs = Host.service client 1000 >>= \case
  (Just (B.Event t p chan dat pack)) -> case t of
    B.None       -> print "got none"       >> return gs
    B.Connect    -> print "got connect"    >> return gs
    B.Disconnect -> print "got disconnect" >> return gs
    B.Receive    -> print "got receive"    >> return gs
  Nothing        -> print "no packet"      >> return gs
