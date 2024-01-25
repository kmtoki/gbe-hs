module Gameboy where

import Gameboy.Cartrige
import Gameboy.CPU
import Gameboy.MBC
import Gameboy.Memory
import Gameboy.Logger
import Gameboy.Utils

import qualified Data.Vector as V

type Gameboy a = StateT GameboyState IO a
--type CPU a = StateT CPUState (StateT MBCState (StateT LoggerState IO)) a

data GameboyState = GameboyState {
    _cpu :: CPUState,
    _mbc :: MBCState,
    _logger :: LoggerState,
    _car :: Cartrige
  }

makeLenses ''GameboyState

newGameboyState :: String -> IO GameboyState
newGameboyState gb = do
  let cpu' = newCPUState
  logger' <- newLoggerState 3 False False 0xfff
  car' <- readCartrige gb
  mbc' <- newMBCState car'

  pure $ GameboyState cpu' mbc' logger' car'

runGameboy :: GameboyState -> IO GameboyState
runGameboy gb = do
  let run = flip runStateT
  (((_,cpu'),mbc'),logger') <- run (gb^.logger) $ run (gb^.mbc) $ run (gb^.cpu) executeCPU
  pure $ gb 
    & (cpu .~ cpu')
    . (mbc .~ mbc')
    . (logger .~ logger')

serialToString :: GameboyState -> String
serialToString gb = gb^.cpu^.serial_buffer & V.map (chr.fi) & V.toList
