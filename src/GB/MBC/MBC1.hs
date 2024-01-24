module GB.MBC.MBC1 (newMBC1, readMBC1, writeMBC1) where

import GB.MBC.Internal
import GB.Cartridge
import GB.Utils

import Data.Vector.Unboxed qualified as V

data MBCRegisters 
  = ROMBank1 | ROMBank2 | ROMBank 
  | RAMExEnable | RAMExBank 
  | BankingMode 
  | VRAMBlocking | OAMBlocking 
  deriving (Enum, Show)

readReg :: MBC -> MBCRegisters -> IO Word64
readReg mbc@(MBC {..}) r = readStore regs $ fromEnum r

writeReg :: MBC -> MBCRegisters -> Word64 -> IO ()
writeReg mbc@(MBC {..}) r n = writeStore regs (fromEnum r) n

newMBC1 :: Cartridge -> IO MBC
newMBC1 cartridge = do
  regs <- newStore 0xff 0
  ram <- newStore 0x10000 0
  ramEx <- newStore 0x90000 0
  let rom = raw cartridge
  pure $ MBC { .. }

readMBC1 :: MBC -> Word16 -> IO Word8
readMBC1 mbc@(MBC {..}) i
  | i >= 0 && i <= 0x3fff = pure $ rom V.! fi i
  | i >= 0x4000 && i <= 0x7fff = do
    bank <- fi <$> readReg mbc ROMBank
    pure $ rom V.! (bank .|. (fi i - 0x4000))
  | i >= 0x8000 && i <= 0x9fff = readStore ram $ fi i
  | i >= 0xa000 && i <= 0xbfff = do
    enbale <- readReg mbc RAMExEnable
    if (enbale == 1) then do
      bank <- readReg mbc RAMExBank
      readStore ramEx (fi bank .|. (fi i - 0xa000))
    else
      pure 0
  | otherwise = readStore ram $ fi i

writeMBC1 :: MBC -> Word16 -> Word8 -> IO ()
writeMBC1 mbc@(MBC {..}) i n
  | i >= 0 && i <= 0x1fff = writeReg mbc RAMExEnable $ toNum (n .&. 0xf == 0xa)
  | i >= 0x2000 && i <= 0x3fff = do
    writeReg mbc ROMBank1 $ fi $ if n == 0 then 1 else n .&. 0x1f
    b1 <- readReg mbc ROMBank1
    b2 <- readReg mbc ROMBank2
    writeReg mbc ROMBank $ fi b2 `shiftL` 19 .|. fi b1 `shiftL` 14
  | i >= 0x4000 && i <= 0x5fff = do
    writeReg mbc ROMBank2 $ fi n .&. 3
    b1 <- readReg mbc ROMBank1
    b2 <- readReg mbc ROMBank2
    writeReg mbc ROMBank $ fi b2 `shiftL` 19 .|. fi b1 `shiftL` 14
  | i >= 0x6000 && i <= 0x7fff = do
    if n /= 0 then do
      writeReg mbc RAMExEnable 1
      b2 <- readReg mbc ROMBank2
      writeReg mbc RAMExBank $ b2 `shiftL` 13
    else do
      writeReg mbc RAMExEnable 0
      writeReg mbc RAMExBank 0
  | i >= 0x8000 && i <= 0x9fff = do
    b <- readReg mbc VRAMBlocking 
    when (b == 0) $ do
      writeStore ram (fi i) n
  | i >= 0xa000 && i <= 0xbfff = do
    e <- readReg mbc RAMExEnable
    when (e == 1) $ do
      b <- readReg mbc RAMExEnable
      writeStore ramEx (fi b .|. (fi i - 0xa000)) n
  | i == 0xff46 = do
    b <- readReg mbc OAMBlocking
    writeStore ram (fi i) n
    when (b == 0) $ do
      dma <- readStore ram $ toInt DMA
      forM_ [0 .. 0xff] $ \i' -> do
        v <- readStore ram (fi dma `shiftL` 8 + i')
        writeStore ram (0xfe00 + fi i) v
  | otherwise = writeStore ram (fi i) n
