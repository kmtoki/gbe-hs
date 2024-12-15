module GB (GB(..), newGBState, stepGB, serialToString, setLogging, readLog) where

import GB.Cartridge
import GB.CPU
import GB.MBC
import GB.Logger
import GB.Utils

import Prelude hiding (log)

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC


newGBState :: String -> IO GBState
newGBState file = do
  car <- readFileCartridge file
  mbc <- newMBC car
  cpu <- newCPU mbc
  pure $ GBState cpu

stepGB :: GB ()
stepGB = stepCPU

serialToString :: GB String
serialToString = (BC.unpack . B.pack) <$> readSerial

setLogging :: Bool -> GB ()
setLogging = setIsLogging

readLog :: GB String
readLog  = do
  CPU {..} <- getCPU
  [log] <- liftIO $ readsLogger cpuLogger 1
  pure $ showCPULog log 
