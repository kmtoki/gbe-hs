module Debugger where

import Gameboy.Cartrige
import Gameboy.CPU hiding (dispatch)
import Gameboy.MBC
import Gameboy.Memory
import Gameboy.Logger
import Gameboy.Utils

import System.Environment
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Char hiding (isPrint)
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Text.Read as T
import Text.Regex

type Debugger a = StateT DebuggerState (StateT CPUState (StateT MBCState (StateT LoggerState IO))) a

data DebuggerState = DebuggerState {
    _counter :: Int
  }

makeLenses ''DebuggerState

newDebuggerState :: DebuggerState
newDebuggerState = DebuggerState 0

changeLoggerPrint :: Debugger ()
changeLoggerPrint = lift $ lift $ lift $ do
  isPrint %= not
  isp <- use isPrint
  liftIO $ putStrLn ("logger.isPrint -> " ++ show isp)

showSerialBuffer :: Debugger ()
showSerialBuffer = lift $ do
  sb <- use serial_buffer
  liftIO $ putStrLn $ V.toList $ V.map (chr.fromIntegral) sb

readLogList :: Int -> Debugger ()
readLogList n = lift $ lift $ lift $ do
  size <- use bufSize
  buf <- use buffer
  pos <- use pos
  let 
    offset = if pos - n < 0 then size - (pos - n) else pos - n
    go o c =
      if c == n then
        pure ()
      else
        if o < size then do
          liftIO $ do
            l <- VM.read buf o
            putStrLn l
          go (o + 1) (c + 1)
        else
          go 0 c 
 
  go offset 0

dispatch :: [String] -> Debugger ()
dispatch [] = step >> readLogList 2
dispatch (cmd:args) = case cmd of
  "print" -> changeLoggerPrint
  "p" -> changeLoggerPrint
  "s" -> showSerialBuffer
  "q" -> error "exit"
  _ -> do 
    if all isNumber cmd then do
      replicateM_ (T.read cmd) step
    else do
      step

    p <- lift $ lift $ lift $ use isPrint
    when (not p) $ do
      readLogList 2

step :: Debugger ()
step = do
  counter += 1
  lift executeCPU

shell :: Debugger ()
shell = do
  liftIO $ putStr "> "
  cmds <- liftIO $ words <$> getLine
  dispatch cmds

debugger :: String -> IO ()
debugger rom = do
  let 
    debuggerState = newDebuggerState
    cpu = newCPUState
  logger <- newLoggerState 3 False False 0xffff
  car <- readCartrige rom
  mbc <- newMBCState car
  print car
  go debuggerState cpu mbc logger
  where
    go :: DebuggerState -> CPUState -> MBCState -> LoggerState -> IO ()
    go ds cpu mbc logger = do
      let run = flip runStateT
      ((((_,ds'),cpu'),mbc'),logger') <- run logger $ run mbc $ run cpu $ run ds shell
      go ds' cpu' mbc' logger'

main' = debugger "rom/gb_test_roms/cpu_instrs/cpu_instrs.gb"
