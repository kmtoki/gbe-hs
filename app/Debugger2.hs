module Debugger2 where

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

type Debugger a = StateT DebuggerState IO a

data DebuggerState = DebuggerState {
    _cpu :: CPUState,
    _mbc :: MBCState,
    _logger :: LoggerState,
    _car :: Cartrige,
    _counter :: Int
  }

makeLenses ''DebuggerState

newDebuggerState :: String -> IO DebuggerState
newDebuggerState gb = do
  let cpu' = newCPUState
  logger' <- newLoggerState 3 False False 0xfff
  car' <- readCartrige gb
  mbc' <- newMBCState car'

  pure $ DebuggerState cpu' mbc' logger' car' 0

step :: Debugger ()
step = do
  counter += 1
  cpu' <- use cpu
  mbc' <- use mbc
  logger' <- use logger

  let run = flip runStateT
  (((_,cpu''),mbc''),logger'') <- lift $ run logger' $ run mbc' $ run cpu' executeCPU

  cpu .= cpu''
  mbc .= mbc''
  logger .= logger''

showRAM :: Debugger ()
showRAM = do
  r <- use $ mbc.memory.ram
  let 
    width = 0x10
    height = 0x10000 `div` width

  lift $ do
    forM_ [0 .. height - width] $ \y -> do
      putStr $ showHex (y * width) ++ " "
      forM_ [0 .. width] $ \x -> do
        w <- VM.read r (y * width + x)
        putStr $ (showHex' w ++ " ")
      putStrLn ""

showMBCState :: Debugger ()
showMBCState = do
  m <- use mbc
  lift $ print m

changeLoggerLevel :: Int -> Debugger ()
changeLoggerLevel n = do
  logger.level .= n
  lift $ putStrLn $ "change logger level"

changeLoggerPrint :: Debugger ()
changeLoggerPrint = do
  logger.isPrint %= not
  isp <- use $ logger.isPrint
  lift $ putStrLn ("logger.isPrint -> " ++ show isp)

changeLoggerIsLogging :: Debugger ()
changeLoggerIsLogging = do
  logger.isLogging %= not
  isl <- use $ logger.isLogging
  lift $ putStrLn ("logger.isLogging -> " ++ show isl)

readRAM :: Int -> Debugger ()
readRAM i = do
  r <- use $ mbc.memory.ram
  lift $ do
    w <- VM.read r i
    putStrLn $ showHex w

toPC :: Word16 -> Debugger ()
toPC ww = do
  pc' <- use $ cpu.pc
  when (ww /= pc') $ do
    step
    toPC ww

toMatch :: Regex -> Debugger ()
toMatch re = do
  pos <- use $ logger.pos
  buf <- use $ logger.buffer
  a <- VM.read buf pos
  b <- VM.read buf $ if pos - 1 < 0 then 0 else pos - 1
  let f = isNothing . matchRegex re
  when (f a && f b) $ do
    step
    toMatch re
  
readLogList :: Int -> Debugger ()
readLogList n = do
  size <- use $ logger.bufSize
  buf <- use $ logger.buffer
  pos <- use $ logger.pos
  let 
    offset = if pos - n < 0 then size - (pos - n) else pos - n
    go o c =
      if c == n then
        pure ()
      else
        if o < size then do
          lift $ do
            l <- VM.read buf o
            putStrLn l
          go (o + 1) (c + 1)
        else
          go 0 c 
 
  go offset 0

showSerialBuffer :: Debugger ()
showSerialBuffer = do
  sb <- use $ cpu.serial_buffer
  liftIO $ putStrLn $ V.toList $ V.map (chr.fromIntegral) sb


dispatch :: [String] -> Debugger ()
dispatch [] = step >> readLogList 2
dispatch (cmd:args) = case cmd of
  "print" -> changeLoggerPrint
  "p" -> changeLoggerPrint
  "level" -> changeLoggerLevel $ T.read $ head args
  "log" -> changeLoggerIsLogging
  "mbc" -> showMBCState
  "serial" -> showSerialBuffer
  "s" -> showSerialBuffer
  "pc" -> toPC $ T.read $ head args
  "match" -> do
    let re = mkRegex $ head args
    toMatch re
  "ram" -> 
    if length args == 1 && (all isNumber $ head args) then 
      readRAM $ T.read $ head args 
    else 
      showRAM
  "logs" -> 
    if length args == 1 && (all isNumber $ head args) then
      readLogList $ T.read $ head args 
    else 
      readLogList 0xff
  "exit" -> error "exit"
  "q" -> error "quit"
  _ -> do 
    if all isNumber cmd then do
      replicateM_ (T.read cmd) step
    else do
      step

    p <- use $ logger.isPrint
    when (not p) $ do
      readLogList 2



shell :: Debugger ()
shell = do
  cmds <- lift $ words <$> getLine
  dispatch cmds


executeDebugger :: Debugger ()
executeDebugger = do
  --logger.isPrint .= True
  step

debugger :: String -> IO ()
debugger  file = do
  debuggerState <- newDebuggerState file
  print $ debuggerState^.car
  go debuggerState 0
  where
    go ds n = do
      --(_, ds') <- runStateT executeDebugger ds
      (_, ds') <- runStateT shell ds
      --when (n < 10000000) $ do
      go ds' (n + 1)

--main' = debugger "roms/gb_test_roms/cpu_instrs/cpu_instrs.gb"
main' = debugger "C:\\Users\\higum\\Github\\gbe-hs\\roms\\gb-test-roms\\cpu_instrs\\cpu_instrs.gb"

--main :: IO ()
--main = do
--  [file] <- getArgs
--  debugger file

--test' = do
--  let 
--    cpu = newCPUState
--    logger = newLoggerState 3 False True 0xffff
--  car <- readCartrige "roms/gb_test_roms/cpu_instrs/cpu_instrs.gb"
--  mbc <- newMBCState car
--  go 0 cpu mbc logger
--  where
--    go n cpu mbc logger = do
--      let run = flip runStateT
--      (((_,cpu'),mbc'),logger') <- run logger $ run mbc $ run cpu executeCPU
--      
--      if n == 0 then do
--        when (not (logger'^.isPrint)) $ do
--          let b = logger'^.buffer
--          V.mapM_ putStrLn $ V.drop (V.length b - 2) b
--
--        ls <- getLine
--        let n' = if length ls == 0 then 1 else T.read ls
--        go n' cpu' mbc' logger'
--      else
--        go (n - 1) cpu' mbc' logger'
