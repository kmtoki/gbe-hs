module GB.Utils (
  module Data.Bits,
  module Data.Char,
  module Data.Int,
  module Data.Word,
  module Control.Monad,

  Store,
  newStore,
  readStore,
  writeStore,
  modifyStore,

  ROM,
  RAM,

  addCarryHalf,
  subCarryHalf,

  fi,
  chr,
  showHex,
  showHex',
  showHex'',
  showHex16,
  showBin,
  showBin',
  showSignedWord8,

  toWord16,
  sepWord16,
  isZero,
  toNum,

  GBRegisters(..),
  Address,
  toInt
  ) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Char (chr, intToDigit, toUpper)
import Control.Monad
import Numeric qualified as N

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VM


newtype Store a = Store (VM.MVector (VM.PrimState IO) a)

newStore :: VM.Unbox a => Int -> a -> IO (Store a)
newStore size a = do
  v <- VM.replicate size a
  pure $ Store v

readStore :: VM.Unbox a => Store a -> Int -> IO a
readStore (Store v) i = VM.read v i

writeStore :: VM.Unbox a => Store a -> Int -> a -> IO ()
writeStore (Store v) i w = VM.write v i w

modifyStore :: VM.Unbox a => Store a -> Int -> (a -> a) -> IO ()
modifyStore v i f = do
  w <- readStore v i
  writeStore v i $ f w


type RAM = Store Word8
type ROM = VU.Vector Word8


class (Num a, Num b) => AddCarryHalf a b where
  addCarryHalf :: a -> b -> (a, Bool, Bool)

instance AddCarryHalf Word8 Word8 where
  addCarryHalf w w' = (res, w > res, www .&. 0x10 /= 0)
    where
      res = w + w'
      www = w .^. w' .^. res

instance AddCarryHalf Word16 Word16 where
  addCarryHalf w w' = (res, w > res, www .&. 0x1000 /= 0)
    where
      res = w + w'
      www = w .^. w' .^. res

instance AddCarryHalf Word16 Int8 where
  addCarryHalf w16 w8 = (res, www .&. 0x100 /= 0, www .&. 0x10 /= 0)
    where
      i = fi (fi w8 :: Int8) :: Int32
      w = fi i :: Word16
      res = fi ((fi w16 :: Int32) + i) :: Word16
      www = w16 .^. w .^. res

class (Num a, Num b) => SubCarryHalf a b where
  subCarryHalf :: a -> b -> (a, Bool, Bool)

instance SubCarryHalf Word8 Word8 where
  subCarryHalf w w' = (res, res > w, www .&. 0x10 /= 0)
    where
      res = w - w'
      www = w .^. w' .^. res

instance SubCarryHalf Word16 Word16 where
  subCarryHalf w w' = (res, res > w, www .&. 0x1000 /= 0)
    where
      res = w - w'
      www = w .^. w' .^. res


fi :: (Integral a, Integral b) => a -> b
fi = fromIntegral

showHex'' :: (Show a, Integral a) => a -> String
showHex'' x = map toUpper $ N.showHex x ""

showHex :: (Show a, Integral a) => a -> String
--showHex a = "$" ++ showHex'' a
showHex x = N.showHex x ""

showHex' :: (Show a, Integral a) => a -> String
showHex' x = if length s == 1 then "0" ++ s else s
  where
    s = showHex'' x

showHex16 :: (Show a, Integral a) => a -> String
showHex16 a = "$" ++ showHex16' a

showHex16' :: (Show a, Integral a) => a -> String
showHex16' x = if l > 0 then replicate l '0' ++ s else s
  where
    s = showHex'' x
    l = 4 - length s

showBin :: (Show a, Integral a) => a -> String
showBin a = "0b" ++ N.showIntAtBase 2 intToDigit a ""

showBin' :: (Show a, Integral a) => a -> String
showBin' a = N.showIntAtBase 2 intToDigit a ""

showSignedWord8 :: Word8 -> String
showSignedWord8 i = 
  if testBit i 7 then
      "-" ++ show (128 - clearBit i 7)
  else
      "+" ++ show (clearBit i 7)


toWord16 :: Word8 -> Word8 -> Word16
toWord16 h l = (fi h `shiftL` 8) .|. fi l

sepWord16 :: Word16 -> (Word8, Word8)
sepWord16 ww = (fi (ww `shiftR` 8), fi (ww .&. 0b11111111))

isZero :: (Num a, Eq a) => a -> Bool
isZero = (0 ==)

toNum :: Num a => Bool -> a
toNum False = 0
toNum True = 1

data GBRegisters 
  = JOYP
  | SB
  | SC
  | NR10
  | NR11
  | NR12
  | NR13
  | NR14
  | NR21
  | NR22
  | NR23
  | NR24
  | NR30
  | NR31
  | NR32
  | NR33
  | NR34
  | NR41
  | NR42
  | NR43
  | NR44
  | NR50
  | NR51
  | NR52
  | WPR 
  | LCDC
  | STAT
  | SCY
  | SCX
  | LY
  | LYC
  | WY
  | WX
  | BGP
  | OBP0
  | OBP1
  | BCPS
  | BCPD
  | OCPS
  | DMA
  | HDMA1
  | HDMA2
  | HDMA3
  | HDMA4
  | HDMA5
  | VBK
  | DIV
  | TIMA
  | TMA
  | TAC
  | IF
  | IE
  deriving Show

class Address a where
  toInt :: a -> Int

instance Address Int where
  toInt i = i

instance Address Word8 where
  toInt ww = fromIntegral ww

instance Address Word16 where
  toInt ww = fromIntegral ww

instance Address GBRegisters where
  toInt r = case r of
    JOYP -> 0xff00

    SB -> 0xff01
    SC -> 0xff02

    DIV -> 0xff04
    TIMA -> 0xff05
    TMA -> 0xff06
    TAC -> 0xff07

    NR10 -> 0xff10
    NR11 -> 0xff11
    NR12 -> 0xff12
    NR13 -> 0xff13
    NR14 -> 0xff14
    NR21 -> 0xff16
    NR22 -> 0xff17
    NR23 -> 0xff18
    NR24 -> 0xff19
    NR30 -> 0xff1a
    NR31 -> 0xff1b
    NR32 -> 0xff1c
    NR33 -> 0xff1d
    NR34 -> 0xff1e
    NR41 -> 0xff20
    NR42 -> 0xff21
    NR43 -> 0xff22
    NR44 -> 0xff23
    NR50 -> 0xff24
    NR51 -> 0xff25
    NR52 -> 0xff26
    WPR -> 0xff30

    LCDC -> 0xff40
    STAT -> 0xff41
    SCY -> 0xff42
    SCX -> 0xff43
    LY -> 0xff44
    LYC -> 0xff45
    WY -> 0xff4a
    WX -> 0xff4b
    BGP -> 0xff47
    OBP0 -> 0xff48
    OBP1 -> 0xff49
    BCPS -> 0xff68
    BCPD -> 0xff69
    OCPS -> 0xff6a
    DMA -> 0xff46
    VBK -> 0xff4f
    HDMA1 -> 0xff51
    HDMA2 -> 0xff52
    HDMA3 -> 0xff53
    HDMA4 -> 0xff54
    HDMA5 -> 0xff55

    IF -> 0xff0f
    IE -> 0xffff
