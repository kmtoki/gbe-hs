module GB.MBC (MBC, newMBC, readMBC, writeMBC, readGBReg, writeGBReg, modifyGBReg, readMBCROMBank) where

import GB.Cartridge
import GB.Utils

import GB.MBC.Internal
import GB.MBC.MBC1

newMBC :: Cartridge -> IO MBC
newMBC car = case mbcType car of
  MBC1 -> newMBC1 car
  _ -> error "newMBC unimplement MBCType"

readMBC :: MBC -> Word16 -> IO Word8
readMBC mbc@(MBC {..}) i = case mbcType cartridge of
  MBC1 -> readMBC1 mbc i
  _ -> error "readMBC unimplement MBCType"
  
writeMBC :: MBC -> Word16 -> Word8 -> IO ()
writeMBC mbc@(MBC {..}) i n = case mbcType cartridge of
  MBC1 -> writeMBC1 mbc i n
  _ -> error "writeMBC unimplement MBCType"

readGBReg :: MBC -> GBRegisters -> IO Word8
readGBReg mbc r = readMBC mbc $ fi $ toInt r

writeGBReg :: MBC -> GBRegisters -> Word8 -> IO ()
writeGBReg mbc r n = writeMBC mbc (fi $ toInt r) n

modifyGBReg :: MBC -> GBRegisters -> (Word8 -> Word8) -> IO ()
modifyGBReg mbc r f = readGBReg mbc r >>= writeGBReg mbc r . f


readMBCROMBank :: MBC -> IO Word64
readMBCROMBank (MBC {..}) = readStore regs 2
