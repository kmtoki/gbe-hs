module GB (GB(..), newGBState, runGB, stepGB, serialToString, setLogging, readLog) where

import GB.Prelude
import GB.Internal
import GB.Cartridge
import GB.CPU
import GB.MBC
import GB.Logger

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC


newGBState :: String -> IO GBState
newGBState file = do
  car <- readFileCartridge file
  mbc <- newMBC car
  cpu <- newCPU
  pure $ GBState cpu mbc


runGB :: GB a -> GBState -> IO a
runGB (GB gb) gbs = runReaderT gb gbs

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
