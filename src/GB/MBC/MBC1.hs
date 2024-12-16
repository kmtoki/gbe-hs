module GB.MBC.MBC1 (newMBC1, readMBC1, writeMBC1) where

import GB.Prelude hiding (reader, read)
import GB.Internal

import Data.Vector.Unboxed qualified as V

data MBCRegisters 
  = ROMBank1 | ROMBank2 | ROMBank 
  | RAMExEnable | RAMExBank 
  | BankingMode 
  | VRAMBlocking | OAMBlocking 
  deriving (Enum, Show)

readReg :: MBCRegisters -> GB Word64
readReg r = do
  MBC {..} <- getMBC
  readStore regs $ fromEnum r

writeReg :: MBCRegisters -> Word64 -> GB ()
writeReg r n = do
  MBC {..} <- getMBC
  writeStore regs (fromEnum r) n

newMBC1 :: Cartridge -> IO MBC
newMBC1 cartridge = do
  regs <- newStore 0xff 0
  ram <- newStore 0x10000 0
  ramEx <- newStore 0x90000 0
  let
    rom = cartridge.raw
    reader = readMBC1
    writer = writeMBC1
  pure $ MBC { .. }

readMBC1 :: Word16 -> GB Word8
readMBC1 i' = do
  MBC {..} <- getMBC
  let
    read :: Word16 -> GB Word8
    read i
      | i >= 0 && i <= 0x3fff = pure $ rom V.! fi i
      | i >= 0x4000 && i <= 0x7fff = do
        bank <- fi <$> readReg ROMBank
        pure $ rom V.! (bank .|. (fi i - 0x4000))
      | i >= 0x8000 && i <= 0x9fff = readStore ram $ fi i
      | i >= 0xa000 && i <= 0xbfff = do
        enbale <- readReg RAMExEnable
        if (enbale == 1) then do
          bank <- readReg RAMExBank
          readStore ramEx (fi bank .|. (fi i - 0xa000))
        else
          pure 0
      | otherwise = readStore ram $ fi i
  read i'

writeMBC1 :: Word16 -> Word8 -> GB ()
writeMBC1 i' n' = do
  MBC {..} <- getMBC
  let
    write :: Word16 -> Word8 -> GB ()
    write i n 
      | i >= 0 && i <= 0x1fff = writeReg RAMExEnable $ toNum (n .&. 0xf == 0xa)
      | i >= 0x2000 && i <= 0x3fff = do
        writeReg ROMBank1 $ fi $ if n == 0 then 1 else n .&. 0x1f
        b1 <- readReg ROMBank1
        b2 <- readReg ROMBank2
        writeReg ROMBank $ fi b2 `shiftL` 19 .|. fi b1 `shiftL` 14
      | i >= 0x4000 && i <= 0x5fff = do
        writeReg ROMBank2 $ fi n .&. 3
        b1 <- readReg ROMBank1
        b2 <- readReg ROMBank2
        writeReg ROMBank $ fi b2 `shiftL` 19 .|. fi b1 `shiftL` 14
      | i >= 0x6000 && i <= 0x7fff = do
        if n /= 0 then do
          writeReg RAMExEnable 1
          b2 <- readReg ROMBank2
          writeReg RAMExBank $ b2 `shiftL` 13
        else do
          writeReg RAMExEnable 0
          writeReg RAMExBank 0
      | i >= 0x8000 && i <= 0x9fff = do
        b <- readReg VRAMBlocking 
        when (b == 0) $ do
          writeStore ram (fi i) n
      | i >= 0xa000 && i <= 0xbfff = do
        e <- readReg RAMExEnable
        when (e == 1) $ do
          b <- readReg RAMExEnable
          writeStore ramEx (fi b .|. (fi i - 0xa000)) n
      | i == 0xff46 = do
        b <- readReg OAMBlocking
        writeStore ram (fi i) n
        when (b == 0) $ do
          dma <- readStore ram $ toInt DMA
          forM_ [0 .. 0xff] $ \i' -> do
            v <- readStore ram (fi dma `shiftL` 8 + i')
            writeStore ram (0xfe00 + fi i) v
      | otherwise = writeStore ram (fi i) n
  write i' n'
