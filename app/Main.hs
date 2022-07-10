module Main where

import Gameboy.Cartrige
import Gameboy.CPU
import Gameboy.MBC
import Gameboy.Memory
import Gameboy.Logger
import Gameboy.Utils

import Control.Concurrent
import qualified Data.Vector as V

main :: IO ()
main = do
  let 
    logger = newLoggerState True True 0xfff
    cpu = newCPUState

  car <- readCartrige "roms/gb_test_roms/cpu_instrs/cpu_instrs.gb"
  mbc <- newMBCState car

  s1 <- forkIO $ go 0 cpu mbc logger
  s2 <- forkIO $ getLine >> killThread s1
  pure()

  where
    run = flip runStateT
    go x cpu mbc logger = do
      (((_,cpu'),mbc'),logger') <- run logger $ run mbc $ run cpu executeCPU

      go (x + 1) cpu' mbc' logger'
