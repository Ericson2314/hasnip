{-# LANGUAGE MagicHash, BangPatterns #-}
{-# LANGUAGE Rank2Types, ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds, KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -funbox-strict-fields #-}
module HaSnip.Types where

import Control.Concurrent.Chan
import System.IO.Unsafe(unsafePerformIO) -- just for show instance

import GHC.Prim(Constraint)

import Data.IORef
import Data.Map
import Data.Word
import Data.BitSet.Generic
import Data.Bytes.Serial
import GHC.Generics

import Control.Applicative

import Foreign.Storable


data Vect3F = Vect3F
              !Float -- ^ x
              !Float -- ^ y
              !Float -- ^ z
            deriving (Eq, Show, Ord)

instance Serial Vect3F where
  serialize (Vect3F a b c) = serializeLE a >> serializeLE b >> serializeLE c
  deserialize = Vect3F <$> deserializeLE <*> deserializeLE <*> deserializeLE

newtype Position    = Position    Vect3F deriving (Eq, Show, Ord, Serial)
newtype Orientation = Orientation Vect3F deriving (Eq, Show, Ord, Serial)
newtype Velocity    = Velocity    Vect3F deriving (Eq, Show, Ord, Serial)

newtype PlayerID    = PlayerID    Word8  deriving (Eq, Show, Ord, Serial, Storable)
newtype ItemID      = ItemID      Word8  deriving (Eq, Show, Ord, Serial, Storable)
newtype Health      = Health      Word8  deriving (Eq, Show, Ord, Serial, Storable, Enum, Num, Real, Integral)

data    Color       = Color
                      !Word8 -- ^ Blue
                      !Word8 -- ^ Green
                      !Word8 -- ^ Red
                    deriving (Eq, Show, Generic)
instance Serial Color

data    ColorA      = ColorA
                      !Word8 -- ^ Blue
                      !Word8 -- ^ Green
                      !Word8 -- ^ Red
                      !Word8 -- ^ Alpha
                    deriving (Eq, Show, Generic)
instance Serial ColorA

data Team     = Blue | Green | Neutral deriving (Eq, Show, Ord, Enum)

-- don't define instance for Team, no consistant rep

data ItemType = Intel | Home            deriving (Eq, Show, Ord, Enum)

data Thing    = Thing
                !Position
                !Orientation
                !Team
              deriving (Eq, Show)
data Player   = Player
                !Thing
                !Health
                !Color
              deriving (Eq, Show)

data Item = Item Thing ItemType
          deriving (Eq, Show)

data GState = GState              {
  players :: Map PlayerID Player  ,
  items   :: Map ItemID   Item    }

instance Show a => Show (IORef a) where
  show ref = "IORef: " ++ (show $ unsafePerformIO $ readIORef ref)

eden :: GState
eden = GState Data.Map.empty Data.Map.empty

-- | Mailbox for each loop actor thread thing
type EChan (ct :: * -> Constraint) = forall e. ct e => Chan e

-- | Things that can go in Sound's inbox
class Sound   elem state where  yell  :: elem -> state -> GState -> IO ()

-- | Things that can go in Videos's inbox
class Video   elem state where  shine :: elem -> state -> GState -> IO ()

-- | Things that can go in Input's inbox
class Input   elem state where  jab   :: elem -> state -> GState -> IO ()

-- | Things that can go in Network's inbox
class Network elem state where  mail  :: elem -> state -> GState -> IO ()


-- | Things that can go in anybody's mailbox
class Handle elem        where handle :: elem -> IO ()

instance Handle elem => Sound   elem s where yell  e _ _ = handle e
instance Handle elem => Video   elem s where shine e _ _ = handle e
instance Handle elem => Input   elem s where jab   e _ _ = handle e
instance Handle elem => Network elem s where mail  e _ _ = handle e
