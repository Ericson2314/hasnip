{-# LANGUAGE MagicHash, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, DeriveGeneric, FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-} -- BAD IDEA
{-# OPTIONS -funbox-strict-fields #-}
module HaSnip.Protocol.Ben.Parse where

import Control.Applicative

import Data.Word
import Data.Int
import Data.BitSet.Generic
import Data.Bytes.Serial
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Get (MonadGet)
import Data.ByteString

import GHC.Generics

import HaSnip.Types

--------------------------------
-- Helper Types
--------------------------------

data    KeyState    = Up | Down | Left | Right | Crouch | Sneak | Sprint
                    deriving (Eq, Show, Enum)

data    SubWeapon   = Primary | Secondary
                    deriving (Eq, Show, Enum)

data    BodyPart    = Torso | Head | Arms | Legs | Melee
                    deriving (Eq, Show, Enum)

data    PainSource  = Fall | Weapon
                    deriving (Eq, Show, Enum)

data    Tool        = Spade | Block | Gun | Grenade
                    deriving (Eq, Show, Enum)

data    Team        = Blue | Green
                    deriving (Eq, Show, Enum)

data    BlockUpdate = Build | BulletD | SpadeD | GrenadeD
                    deriving (Eq, Show, Enum)

data    Chat        = All | Team | System
                    deriving (Eq, Show, Enum)

data    Weapon      = Rifle | SMG | Shotgun
                    deriving (Eq, Show, Enum)

instance SerialEndian c => Serial (BitSet c a) where
  serialize = serializeLE . getBits
  deserialize = BitSet <$> deserializeLE

instance Serial (BitSet Word8 a) where
  serialize = serialize . getBits
  deserialize = BitSet <$> deserialize

--------------------------------
-- Actual Protocol
--------------------------------

type P0  = Position
type P1  = Orientation

 -- P2 is Version Specific

data P3  = P3 !PlayerID !(BitSet Word8 KeyState)
         deriving (Eq, Show, Generic)
instance Serial P3

data P4  = P4 !PlayerID !(BitSet Word8 SubWeapon)
         deriving (Eq, Show, Generic)
instance Serial P4

 -- P5 is specialized

data P6  = P6 { p6_ID          :: !PlayerID
              , p6_FuseLength  :: !Float
              , p6_Position    :: !Position
              , p6_Orientation :: !Orientation }
         deriving (Eq, Show)

data P7  = P7 !PlayerID Tool
         deriving (Eq, Show)

data P8  = P8 !PlayerID !Color
         deriving (Eq, Show)

data P9  = P9 { p9_ID       :: !PlayerID
              , p9_Team     :: !Int8
              , p9_Weapon   :: !Word8
              , p9_HeldItem :: !Word8
              , p9_Kills    :: !Word16
              , p9_Color    :: !Color
              , p9_Name     :: !String }
         deriving (Eq, Show)

data P10 = P10 { p10_ID     :: !PlayerID
               , p10_Team   :: !Int8
               , p10_Weapon :: !Word8 }
         deriving (Eq, Show)

data P11 = P11 { p11_ID       :: !ItemID
               , p11_Team     :: !Int8
               , p11_Position :: !Position }
         deriving (Eq, Show)

data P12 = P12 { p12_ID       :: !PlayerID
               , p12_Weapon   :: !Word8
               , p12_Team     :: !Int8
               , p12_Position :: !Position
               , p12_Name     :: !String }
         deriving (Eq, Show)

data P13 = P13 !PlayerID !BlockUpdate !Position
         deriving (Eq, Show)

data P14 = P14 { p14_ID    :: !PlayerID
               , p14_Start :: !Position
               , p14_End   :: !Position }
         deriving (Eq, Show)

 -- Gap

data P17 = P17 !PlayerID !Chat !String
         deriving (Eq, Show)

 -- P18 is Version Specific
 -- Gap

data P28 = P28 { p28_ID      :: !PlayerID
               , p28_Clip    :: !Word8
               , p28_Reserve :: !Word8 }
     deriving (Eq, Show)

 -- Gap

data P30 = P30 !PlayerID !Weapon
         deriving (Eq, Show)

data InPacket world map
  = I0  !P0
  | I1  !P1
  | I2  !world
  | I3  !P3
  | I4  !P4
  | I5  !PlayerID !BodyPart        -- ^ player damaged
  | I6  !P6
  | I7  !P7
  | I8  !P8
  | I9  !P9
  | I10 !P10
  | I11 !P11
  | I12 !P12
  | I13 !P13
  | I14 !P14

  | I17 !P17
  | I18 !map
  | I19 !ByteString
  | I20 !PlayerID                  -- ^ Leaving player
  | I21 { i21_PID     :: !PlayerID
        , i21_EID     :: !Word8
        , i21_Winning :: !Word8
        , i21_state   :: !Word8 }

  | I23 !PlayerID !Word8
  | I24 !PlayerID                  -- ^ Intel capture
  | I25 !PlayerID !Position        -- ^ Intel drop
  | I26 !PlayerID                  -- ^ Restocking player
  | I27 !ColorA                    -- ^ Fog color
  | I28 !P28

  | I30 !P30
    deriving (Eq, Show)

put :: (Data.Bytes.Put.MonadPut m, Serial a) => a -> m ()
put = serialize
get :: (Data.Bytes.Get.MonadGet m, Serial a) => m a
get = deserialize

instance (Serial world, Serial map) => Serial (InPacket world map) where

  serialize = fail "the server serializes/sends this type, not you!"

  deserialize = (get :: MonadGet m => m Word8) >>= \case
    0 -> I0 <$> get
    1 -> I1 <$> get
    2 -> I2 <$> get
    3 -> I3 <$> get
    4 -> I4 <$> get


data OutPacket world
 = O0  Position
  | O1  P1
  | O2  world
  | O3  P3
  | O4  P4
  | O5  !Health !PainSource !Position
  | O6  !P6
  | O7  !P7
  | O8  !P8
  | O9  !P9
  | O10 !P10
  | O11 !P11
  | O12 !P12
  | O13 !P13
  | O14 !P14

  | O17 !P17

  | O28 !P28
  | O29 !PlayerID !Word8           -- ^ Change Team
  | O30 !P30
    deriving (Eq, Show)

instance (Serial world) => Serial (OutPacket world) where

  deserialize = fail "the server deserializes/receives this type, not you!"

  serialize (O0 p ) = put (0 :: Word8) >> put p
  serialize (O1 o ) = put (1 :: Word8) >> put o
  serialize (O2 w ) = put (2 :: Word8) >> put w
  serialize (O3 p3) = put (3 :: Word8) >> put p3
  serialize (O4 p4) = put (4 :: Word8) >> put p4
