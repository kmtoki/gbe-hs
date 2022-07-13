module Main where

import Gameboy.Cartrige
import Gameboy.CPU
import Gameboy.MBC
import Gameboy.Memory
import Gameboy.Logger
import Gameboy.Utils

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Vector as V

main' :: Int -> IO ()
main' n = do
  let 
    logger = newLoggerState n True True 0xfff
    cpu = newCPUState

  car <- readCartrige "roms/gb_test_roms/cpu_instrs/cpu_instrs.gb"
  mbc <- newMBCState car

  s1 <- forkIO $ do
    go 0 cpu mbc logger

  getLine >> killThread s1

  where
    run = flip runStateT
    go x cpu mbc logger = do
      (((_,cpu'),mbc'),logger') <- run logger $ run mbc $ run cpu executeCPU

      go (x + 1) cpu' mbc' logger'


main :: IO ()
main = main' 4
