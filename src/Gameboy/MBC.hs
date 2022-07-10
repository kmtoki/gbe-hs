module Gameboy.MBC where

import Gameboy.Cartrige
import Gameboy.Memory
import Gameboy.Logger
import Gameboy.Utils

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM


type MBC a = StateT MBCState (StateT LoggerState IO) a

--newtype MBC m a = MBC {
--    runMBC :: StateT MBCState (Logger m) a
--  }
--  deriving (Functor, Applicative, Monad, MonadState (MBCState m), MonadTrans, MonadIO)

data MBCState = MBCState {
    _mbcnState :: MBCNState,
    _memory :: Memory,
    _reader :: Int -> MBC Word8,
    _writer :: Int -> Word8 -> MBC ()
  }

instance Show MBCState where
  show (MBCState s m r w) = "MBCState { _memory = " ++ show m ++ ", mbcnState = " ++ show s ++ " }"

data MBCNState
  = MBC0State
  | MBC1State {
    _bank :: Int,
    _bank1 :: Int,
    _bank2 :: Int,
    _bankRAMX :: Int,
    _enableRAMX :: Bool,
    _bankingMode :: Bool
  } 
  deriving Show


makeLenses ''MBCState
makeLenses ''MBCNState

--newtype MBCT m a = MBCT { runMBCT :: StateT MBCState m a }
--  deriving (Functor,Applicative,Monad)
--
--instance Monad m => MonadState MBCState (MBCT m) where
--  get = MBCT get
--  put = MBCT . put
--
--instance MonadTrans MBCT where
--  lift m = MBCT $ StateT $ \s -> do
--    a <- m
--    pure (a,s)
--
--instance MonadIO m => MonadIO (MBCT m) where
--  liftIO = lift . liftIO

newMBCState :: Cartrige -> IO MBCState
newMBCState car = do
  _memory <- newMemory car
  pure $ MBCState { .. }
  where
    (_mbcnState, _reader, _writer) = case car^.mbcType of
      MBC0 -> (MBC0State, readMBC0, writeMBC0)
      MBC1 -> (MBC1State 0 0 0 0 False False, readMBC1, writeMBC1)
      _ -> undefined

readMBC0 :: Int -> MBC Word8
readMBC0 i
  | 0 <= i && i <= 0x7fff = (V.! i) <$> (use $ memory.cartrige.rom)
  | otherwise = do
    ram' <- use $ memory.ram
    lift $ VM.read ram' i

writeMBC0 :: Int -> Word8 -> MBC ()
writeMBC0 i a = do
  ram' <- use $ memory.ram
  lift $ VM.write ram' i a

readMBC1 :: Int -> MBC Word8
readMBC1 i
  | 0 <= i && i <= 0x3fff = (V.! i) <$> (use $ memory.cartrige.rom)

  | 0x4000 <= i && i <= 0x7fff = do
    (Just b) <- preuse $ mbcnState.bank
    rom' <- use $ memory.cartrige.rom
    pure (rom' V.! (b .|. (i - 0x4000)))

  | 0x8000 <= i && i <= 0x9fff = do
    ram' <- use $ memory.ram
    lift $ VM.read ram' i

  | 0xa000 <= i && i <= 0xbfff = do
    ramx' <- use $ memory.ramx
    (Just b) <- preuse $ mbcnState.bankRAMX
    lift $ VM.read ramx' (b .|. (i - 0xa000))

  | otherwise = do
    ram' <- use $ memory.ram
    lift $ VM.read ram' i

writeMBC1 :: Int -> Word8 -> MBC ()
writeMBC1 i w
  | 0x0 <= i && i <= 0x1fff = do
    mbcnState.enableRAMX .= (w == 0xa)
    ram' <- use $ memory.ram
    lift $ VM.write ram' i w

  | 0x2000 <= i && i <= 0x3fff = do
    if w <= 0x1f then do
      mbcnState.bank1 .= fi w
      (Just b1) <- preuse $ mbcnState.bank1
      (Just b2) <- preuse $ mbcnState.bank2
      let 
        b' = shift b2 19 .|. shift b1 14
        b = if b' == 0x8000 || b' == 0x100000 || b' == 0x180000 then b' + 1 else b'
      mbcnState.bank .= b
      ram' <- use $ memory.ram
      lift $ VM.write ram' i w
    else
      pure ()
    
  | 0x4000 <= i && i <= 0x5fff = do
    mbcnState.bank2 .= fi w
    (Just b1) <- preuse $ mbcnState.bank1
    (Just b2) <- preuse $ mbcnState.bank2
    let 
      b' = shift b2 19 .|. shift b1 14
      b = if b' == 0x8000 || b' == 0x100000 || b' == 0x180000 then b' + 1 else b'
    mbcnState.bank .= b
    ram' <- use $ memory.ram
    lift $ VM.write ram' i w

  | 0x6000 <= i && i <= 0x7fff = do
    if w == 0x1 then do
      mbcnState.bankingMode .= True
      (Just b2) <- preuse $ mbcnState.bank2
      mbcnState.bankRAMX .= shift b2 13
    else do
      mbcnState.bankingMode .= False
      mbcnState.bankRAMX .= 0
    ram' <- use $ memory.ram
    lift $ VM.write ram' i w

  | otherwise = do
    ram' <- use $ memory.ram
    lift $ VM.write ram' i w
