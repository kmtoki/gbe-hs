module GB.MBC (
  MBC(..), 
  newMBC,
  readMBC,
  writeMBC,
  readGBReg,
  writeGBReg,
  modifyGBReg,
  readMBCROMBank
  ) where

import GB.Prelude
import GB.Internal
import GB.Cartridge

import GB.MBC.MBC1

newMBC :: Cartridge -> IO MBC
newMBC car = case car.mbcType of
  MBC1 -> newMBC1 car
  _ -> error "newMBC unimplement MBCType"

readMBC :: Word16 -> GB Word8
readMBC i = do
  mbc <- getMBC
  mbc.reader i
  
writeMBC :: Word16 -> Word8 -> GB ()
writeMBC i n = do
  mbc <- getMBC
  mbc.writer i n

readGBReg :: GBRegisters -> GB Word8
readGBReg r = readMBC $ fi $ toInt r

writeGBReg :: GBRegisters -> Word8 -> GB ()
writeGBReg r n = writeMBC (fi $ toInt r) n

modifyGBReg :: GBRegisters -> (Word8 -> Word8) -> GB ()
modifyGBReg r f = do
  a <- readGBReg r
  writeGBReg r $ f a


readMBCROMBank :: GB Word64
readMBCROMBank = do
  mbc <- getMBC
  readStore mbc.regs 2
