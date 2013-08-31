{-# LANGUAGE MagicHash, BangPatterns #-}
{-# LANGUAGE Rank2Types, ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds, KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -funbox-strict-fields #-}
module HaSnip.Types where

import Control.Concurrent.Chan
import System.IO.Unsafe(unsafePerformIO) -- just for show instance

import GHC.Prim(Constraint)

import Data.IORef
import Data.Map
import Data.Word
import Data.BitSet.Generic

data    Vect3F      = Vect3
                      !Float -- ^ x
                      !Float -- ^ y
                      !Float -- ^ z
                      deriving (Eq, Show, Ord)

newtype Position    = Position    Vect3F deriving (Eq, Show, Ord)
newtype Orientation = Orientation Vect3F deriving (Eq, Show, Ord)
newtype Velocity    = Velocity    Vect3F deriving (Eq, Show, Ord)

newtype PlayerID    = PlayerID    Word8  deriving (Eq, Show, Ord)
newtype ItemID      = ItemID      Word8  deriving (Eq, Show, Ord)
newtype Health      = Health      Word8  deriving (Eq, Show, Ord, Enum, Num, Real, Integral)

data    Color       = Color
                      !Word8 -- ^ Blue
                      !Word8 -- ^ Green
                      !Word8 -- ^ Red
                    deriving (Eq, Show)

data    ColorA      = ColorA
                      !Word8 -- ^ Blue
                      !Word8 -- ^ Green
                      !Word8 -- ^ Red
                      !Word8 -- ^ Alpha
                    deriving (Eq, Show)

data Team     = Blue | Greeen | Neutral deriving (Eq, Show, Ord, Enum)

data ItemType = Intel | Home            deriving (Eq, Show, Ord, Enum)

data Thing    = Thing
                !(IORef Position)
                !(IORef Orientation)
                !Team
              deriving (Eq, Show)
data Player   = Player
                !Thing
                !(IORef Health)
                !Color
              deriving (Eq, Show)

data Item = Item Thing ItemType
          deriving (Eq, Show)

data GState = GState              {
  players :: Map PlayerID Player  ,
  items   :: Map ItemID   Item    }

instance Show a => Show (IORef a) where
  show ref = "IORef: " ++ (show $ unsafePerformIO $ readIORef ref)
