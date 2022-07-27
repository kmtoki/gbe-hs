module Gameboy.Memory where

import Gameboy.Internal
import Gameboy.Utils
import Gameboy.Cartrige

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

newMemory :: Cartrige -> IO Memory
newMemory car = do
  ram' <- VM.replicate 0x10000 0
  ramx' <- VM.replicate (car^.ramxSize) 0
  pure $ Memory car ram' ramx'
