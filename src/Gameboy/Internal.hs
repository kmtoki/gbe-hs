module Gameboy.Internal where

import Gameboy.Cartrige
import Gameboy.Utils

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type GB a = StateT GBState IO a

data GBState = GBState {
  _cpu :: CPUState,
  _mbc :: MBCState,
  _logger :: LoggerState
  }

data Memory = 
  Memory {
    _cartrige :: Cartrige,
    _ram :: RAM,
    _ramx :: RAM
  }

data CPUState = CPUState {
    _a, __f, _b, _c, _d, _e, _h, _l :: Word8,
    _sp, _pc :: Word16,
    --__zero, __negative, __half, __carry :: Bool,
    _serial_counter :: Word8,
    _serial_buffer :: V.Vector Word8,
    _sys_counter :: Word16,
    _ime :: Bool,
    --_ime_prev :: Bool,
    _halting :: Bool,
    _stoping :: Bool,
    _cycle :: Int,
    _cycleM :: Int,
    _exe_counter :: Word64
  } deriving Show

data MBCState = MBCState {
    _mbcnState :: MBCNState,
    _memory :: Memory,
    _reader :: Int -> GB Word8,
    _writer :: Int -> Word8 -> GB ()
  }

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

data LoggerState = LoggerState {
    _level :: Int,
    _isLogging :: Bool,
    _isPrint :: Bool,
    _bufSize :: Int,
    _pos :: Int,
    _buffer :: VM.IOVector String
  }

makeLenses ''GBState
makeLenses ''Memory
makeLenses ''CPUState
makeLenses ''MBCState
makeLenses ''MBCNState
makeLenses ''LoggerState


instance Show MBCState where
  show (MBCState s m r w) = "MBCState { _memory = " ++ show m ++ ", mbcnState = " ++ show s ++ " }"

instance Show Memory where
  show m = "Memory RAM RAMX ROM Cartrige[" ++ c ++ "]"
    where
      c =  show (m^.cartrige^.mbcType) ++ "," ++ showHex (m^.cartrige^.rom&V.length)


