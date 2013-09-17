{-# LANGUAGE MagicHash, BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables, LambdaCase, DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS -funbox-strict-fields #-}
module HaSnip.Protocol.Ben.Parse where

import Control.Applicative

import Data.Word
import Data.Int
import Data.BitSet.Generic
import Data.Bytes.Serial
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Get (MonadGet, getByteString, remaining)
import Data.ByteString

import GHC.Generics

import HaSnip.Types

--------------------------------
-- Helper Functions
--------------------------------

put :: (MonadPut m, Serial a) => a -> m ()
put = serialize
{-# INLINE put #-}

get :: (MonadGet m, Serial a) => m a
get = deserialize
{-# INLINE get #-}

put8 :: forall m n. (MonadPut m, Integral n) => n -> m ()
put8 = put . (fromIntegral :: n -> Word8)
{-# INLINE put8 #-}

get8 :: forall m n. (MonadGet m, Num n) => m n
get8 = fmap fromIntegral (get :: m Word8)
{-# INLINE get8 #-}

putE :: (Enum a, MonadPut m) => a -> m ()
putE = put8 . fromEnum
{-# INLINE putE #-}

getE :: (Enum b, MonadGet m) => m b
getE = toEnum <$> get8
{-# INLINE getE #-}


-- ignore spectators
putTeamB :: MonadPut m => Team -> m ()
putTeamB = put8 . \case
  Blue  -> 0
  Green -> 1

getTeamB :: MonadGet m => m Team
getTeamB = get8 >>= return . \case
            0 -> Blue
            1 -> Green

-- ignore spectators
putTeamT2 :: MonadPut m => Team -> m ()
putTeamT2 = put8 . \case
  Blue    -> 0
  Green   -> 1
  Neutral -> 2

getTeamT2 :: MonadGet m => m Team
getTeamT2 = get8 >>= return . \case
            0 -> Blue
            1 -> Green
            2 -> Neutral

putTeamTM :: MonadPut m => Team -> m ()
putTeamTM = put8 . \case
  Blue    -> 0
  Green   -> 1
  Neutral -> -1

getTeamTM :: MonadGet m => m Team
getTeamTM = get8 >>= return . \case
            0  -> Blue
            1  -> Green
            -1 -> Neutral

putCString :: MonadPut m => String -> m ()
putCString = put

getCString :: MonadGet m => m String
getCString = get

--------------------------------
-- Helper Types
--------------------------------

data    KeyState    = Up | Down | Left | Right | Crouch | Sneak | Sprint
                    deriving (Eq, Show, Enum)

data    SubWeapon   = Primary | Secondary
                    deriving (Eq, Show, Enum)

data    BodyPart    = Torso | Head | Arms | Legs | Melee
                    deriving (Eq, Show, Enum)

instance Serial BodyPart where
  serialize = putE
  deserialize = getE

data    PainSource  = Fall | Weapon
                    deriving (Eq, Show, Enum)

instance Serial PainSource where
  serialize = putE
  deserialize = getE

data    Tool        = Spade | Block | Gun | Grenade
                    deriving (Eq, Show, Enum)

instance Serial Tool where
  serialize = putE
  deserialize = getE

data    BlockUpdate = Build | BulletD | SpadeD | GrenadeD
                    deriving (Eq, Show, Enum)

instance Serial BlockUpdate where
  serialize = putE
  deserialize = getE

data    Chat        = All | Team | System
                    deriving (Eq, Show, Enum)

instance Serial Chat where
  serialize = putE
  deserialize = getE

data    Weapon      = Rifle | SMG | Shotgun
                    deriving (Eq, Show, Enum)

instance Serial Weapon where
  serialize = putE
  deserialize = getE

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

instance Serial P6 where
  serialize (P6 i f p o) = put i >> serializeLE f >> put p >> put o
  deserialize = P6 <$> get <*> deserializeLE <*> get <*> get

data P7  = P7 !PlayerID Tool
         deriving (Eq, Show, Generic)
instance Serial P7

data P8  = P8 !PlayerID !Color
         deriving (Eq, Show, Generic)
instance Serial P8

data P9  = P9 { p9_ID       :: !PlayerID
              , p9_Team     :: !Team
              , p9_Weapon   :: !Word8
              , p9_HeldItem :: !Word8
              , p9_Kills    :: !Word16
              , p9_Color    :: !Color
              , p9_Name     :: !String }
         deriving (Eq, Show)

instance Serial P9 where
  serialize (P9 i t w h k c n) = put i >> putTeamB t >> put w >>
                                 put h >> put k >> put c >> putCString n
  deserialize = P9 <$> get <*> getTeamB <*> get <*> get <*> get <*> get <*> getCString

data P10 = P10 { p10_ID     :: !PlayerID
               , p10_Team   :: !Team
               , p10_Weapon :: !Word8 }
         deriving (Eq, Show)

instance Serial P10 where
  serialize (P10 i t w) = put i >> putTeamB t >> put w
  deserialize = P10 <$> get <*> getTeamB <*> get

data P11 = P11 { p11_ID       :: !ItemID
               , p11_Team     :: !Team
               , p11_Position :: !Position }
         deriving (Eq, Show)

instance Serial P11 where
  serialize (P11 i t p) = put i >> putTeamT2 t >> put p
  deserialize = P11 <$> get <*> getTeamT2 <*> get

data P12 = P12 { p12_ID       :: !PlayerID
               , p12_Weapon   :: !Word8
               , p12_Team     :: !Team
               , p12_Position :: !Position
               , p12_Name     :: !String }
         deriving (Eq, Show)

instance Serial P12 where
  serialize (P12 i w t p n) = put i >> put w >> putTeamB t >> put p >> putCString n
  deserialize = P12 <$> get <*> get <*> getTeamB <*> get <*> getCString

data P13 = P13 !PlayerID !BlockUpdate !Position
         deriving (Eq, Show, Generic)
instance Serial P13

data P14 = P14 { p14_ID    :: !PlayerID
               , p14_Start :: !Position
               , p14_End   :: !Position }
         deriving (Eq, Show, Generic)
instance Serial P14

 -- Gap

data P17 = P17 !PlayerID !Chat !String
         deriving (Eq, Show)

instance Serial P17 where
  serialize (P17 i c s) = put i >> put c >> putCString s
  deserialize = P17 <$> get <*> get <*> getCString

 -- P18 is Version Specific
 -- Gap

data P28 = P28 { p28_ID      :: !PlayerID
               , p28_Clip    :: !Word8
               , p28_Reserve :: !Word8 }
     deriving (Eq, Show, Generic)
instance Serial P28

 -- Gap

data P30 = P30 !PlayerID !Weapon
         deriving (Eq, Show, Generic)
instance Serial P30

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

  | I23 !PlayerID !Bool            -- ^ Intel capture, caused a victory?
  | I24 !PlayerID                  -- ^ Intel pickup
  | I25 !PlayerID !Position        -- ^ Intel drop
  | I26 !PlayerID                  -- ^ Restocking player
  | I27 !ColorA                    -- ^ Fog color
  | I28 !P28

  | I30 !P30
    deriving (Eq, Show)

instance (Serial world, Serial map) => Serial (InPacket world map) where

  serialize = fail "the server serializes/sends this type, not you!"

  deserialize = get8 >>= \case
    0  -> I0  <$> get
    1  -> I1  <$> get
    2  -> I2  <$> get
    3  -> I3  <$> get
    4  -> I4  <$> get
    5  -> I5  <$> get <*> get
    6  -> I6  <$> get
    7  -> I7  <$> get
    8  -> I8  <$> get
    9  -> I9  <$> get
    10 -> I10 <$> get
    11 -> I11 <$> get
    12 -> I12 <$> get
    13 -> I13 <$> get
    14 -> I14 <$> get

    17 -> I17 <$> get
    18 -> I18 <$> get
    19 -> I19 <$> (getByteString =<< fromIntegral <$> remaining)
    20 -> I20 <$> get
    21 -> I21 <$> get <*> get <*> get <*> get

    23 -> I23 <$> get <*> get
    24 -> I24 <$> get
    25 -> I25 <$> get <*> get
    26 -> I26 <$> get
    27 -> I27 <$> get
    28 -> I28 <$> get

    30 -> I30 <$> get

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
  | O29 !PlayerID !Team            -- ^ Change Team
  | O30 !P30
    deriving (Eq, Show)

instance (Serial world) => Serial (OutPacket world) where

  deserialize = fail "the server deserializes/receives this type, not you!"

  serialize (O0  p1 ) = put (0  :: Word8) >> put p1
  serialize (O1  p2 ) = put (1  :: Word8) >> put p2
  serialize (O2  p3 ) = put (2  :: Word8) >> put p3
  serialize (O3  p3 ) = put (3  :: Word8) >> put p3
  serialize (O4  p4 ) = put (4  :: Word8) >> put p4
  serialize (O5 i s p) = put (5  :: Word8) >> put i >> put s >> put p
  serialize (O6  p6 ) = put (6  :: Word8) >> put p6
  serialize (O7  p7 ) = put (7  :: Word8) >> put p7
  serialize (O8  p8 ) = put (8  :: Word8) >> put p8
  serialize (O9  p9 ) = put (9  :: Word8) >> put p9
  serialize (O10 p10) = put (10 :: Word8) >> put p10
  serialize (O11 p11) = put (11 :: Word8) >> put p11
  serialize (O12 p12) = put (12 :: Word8) >> put p12
  serialize (O13 p13) = put (13 :: Word8) >> put p13
  serialize (O14 p14) = put (14 :: Word8) >> put p14

  serialize (O17 p17) = put (17 :: Word8) >> put p17

  serialize (O28 p28) = put (28 :: Word8) >> put p28
  serialize (O29 i t) = put (29 :: Word8) >> put i >> putTeamTM t
  serialize (O30 p30) = put (30 :: Word8) >> put p30
