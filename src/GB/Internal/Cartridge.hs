module GB.Internal.Cartridge (
  CGBFlag(..),
  SGBFlag(..),
  MBCType(..),
  DestinationCode(..)
  ) where

import GB.Prelude

data CGBFlag = CGBOnly | CGBSupport
  deriving Show

data SGBFlag = SGBSupport | NotSGBSupport
  deriving Show

data MBCType
  = MBC0
  | MBC1
  | MBC1_RAM
  | MBC1_RAM_BATTERY
  | MBC2
  | MBC2_BATTERY
  | ROM_RAM
  | ROM_RAM_BATTERY
  | MMM01
  | MMM01_RAM
  | MMM01_RAM_BATTERY
  | MBC3_TIMER_BATTERY
  | MBC3_TIMER_RAM_BATTERY
  | MBC3
  | MBC3_RAM
  | MBC3_RAM_BATTERY
  | MBC5
  | MBC5_RAM
  | MBC5_RAM_BATTERY
  | MBC5_RUMBLE
  | MBC5_RUMBLE_RAM
  | MBC5_RUMBLE_RAM_BATTERY
  | MBC6
  | MBC7_SENSOR_RUMBLE_RAM_BATTERY
  | POCKET_CAMERA
  | BANDAI_TAMA5
  | HuC3
  | HuC1_RAM_BATTERY
  deriving (Show, Enum)

data DestinationCode = Japanese | NonJapanese
  deriving Show

