module Gameboy.Memory where

import Gameboy.Utils
import Gameboy.Cartrige
import Gameboy.Logger

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

data Memory = 
  Memory {
    _cartrige :: Cartrige,
    _ram :: RAM,
    _ramx :: RAM
  }

makeLenses ''Memory

instance Show Memory where
  show m = "Memory RAM RAMX ROM Cartrige[" ++ c ++ "]"
    where
      c =  show (m^.cartrige^.mbcType) ++ "," ++ showHex (m^.cartrige^.rom&V.length)


newMemory :: Cartrige -> IO Memory
newMemory car = do
  ram' <- VM.replicate 0x10000 0
  ramx' <- VM.replicate (car^.ramxSize) 0
  pure $ Memory car ram' ramx'
