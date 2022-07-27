module Gameboy.MBC where

import Gameboy.Internal
import Gameboy.Cartrige
import Gameboy.Memory
import Gameboy.Logger
import Gameboy.Utils

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM


newMBCState :: Cartrige -> IO MBCState
newMBCState car = do
  _memory <- newMemory car
  pure $ MBCState { .. }
  where
    (_mbcnState, _reader, _writer) = case car^.mbcType of
      MBC0 -> (MBC0State, readMBC0, writeMBC0)
      MBC1 -> (MBC1State 0x4000 1 0 0 False False, readMBC1, writeMBC1)
      _ -> undefined

readMBC0 :: Int -> GB Word8
readMBC0 i
  | 0 <= i && i <= 0x7fff = (V.! i) <$> (use $ mbc.memory.cartrige.rom)
  | otherwise = do
    ram' <- use $ mbc.memory.ram
    lift $ VM.read ram' i

writeMBC0 :: Int -> Word8 -> GB ()
writeMBC0 i a = do
  ram' <- use $ mbc.memory.ram
  lift $ VM.write ram' i a

readMBC1 :: Int -> GB Word8
readMBC1 i
  | 0 <= i && i <= 0x3fff = (V.! i) <$> (use $ mbc.memory.cartrige.rom)

  | 0x4000 <= i && i <= 0x7fff = do
    (Just b) <- preuse $ mbc.mbcnState.bank
    rom' <- use $ mbc.memory.cartrige.rom
    pure (rom' V.! (b .|. (i - 0x4000)))

  | 0x8000 <= i && i <= 0x9fff = do
    ram' <- use $ mbc.memory.ram
    lift $ VM.read ram' i

  | 0xa000 <= i && i <= 0xbfff = do
    ramx' <- use $ mbc.memory.ramx
    (Just b) <- preuse $ mbc.mbcnState.bankRAMX
    if b == 0 then do
      ram' <- use $ mbc.memory.ram
      lift $ VM.read ram' i
    else 
      lift $ VM.read ramx' (b .|. (i - 0xa000))

  | otherwise = do
    ram' <- use $ mbc.memory.ram
    lift $ VM.read ram' i

writeMBC1 :: Int -> Word8 -> GB ()
writeMBC1 i w
  | 0x0 <= i && i <= 0x1fff = do
    mbc.mbcnState.enableRAMX .= (w == 0xa)
    ram' <- use $ mbc.memory.ram
    lift $ VM.write ram' i w

  | 0x2000 <= i && i <= 0x3fff = do
    let w' = if w == 0 then 1 else (w .&. 0x1f)
    mbc.mbcnState.bank1 .= fi w'
    (Just b1) <- preuse $ mbc.mbcnState.bank1
    (Just b2) <- preuse $ mbc.mbcnState.bank2
    let 
      b' = shift b2 19 .|. shift b1 14
      b = if b' == 0x8000 || b' == 0x100000 || b' == 0x180000 then b' + 0x4000 else b'
    mbc.mbcnState.bank .= b
    ram' <- use $ mbc.memory.ram
    lift $ VM.write ram' i w'
    logging 3 $ "MBC1: bank1 to " ++ showHex' w
    
  | 0x4000 <= i && i <= 0x5fff = do
    mbc.mbcnState.bank2 .= fi w
    (Just b1) <- preuse $ mbc.mbcnState.bank1
    (Just b2) <- preuse $ mbc.mbcnState.bank2
    let 
      b' = shift b2 19 .|. shift b1 14
      b = if b' == 0x8000 || b' == 0x100000 || b' == 0x180000 then b' + 0x4000 else b'
    mbc.mbcnState.bank .= b
    ram' <- use $ mbc.memory.ram
    lift $ VM.write ram' i w
    logging 3 $ "MBC1: bank2 to " ++ showHex' w

  | 0x6000 <= i && i <= 0x7fff = do
    if w == 0x1 then do
      mbc.mbcnState.bankingMode .= True
      (Just b2) <- preuse $ mbc.mbcnState.bank2
      mbc.mbcnState.bankRAMX .= shift b2 13
    else do
      mbc.mbcnState.bankingMode .= False
      mbc.mbcnState.bankRAMX .= 0
    ram' <- use $ mbc.memory.ram
    lift $ VM.write ram' i w

  | 0xa000 <= i && i <= 0xbfff = do
    (Just e) <- preuse $ mbc.mbcnState.enableRAMX
    if e then do
      ramx' <- use $ mbc.memory.ramx
      lift $ VM.write ramx' (i - 0xa000) w
    else do
      ram' <- use $ mbc.memory.ram
      lift $ VM.write ram' i w
      
  | 0xc000 <= i && i <= 0xddff = do
    ram' <- use $ mbc.memory.ram
    lift $ VM.write ram' i w
    lift $ VM.write ram' (i + 0x2000) w

  | otherwise = do
    ram' <- use $ mbc.memory.ram
    lift $ VM.write ram' i w
