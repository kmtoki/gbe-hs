module GB (GB(..), newGB, stepGB, serialToString, setLogging, readLog) where

import GB.Cartridge
import GB.CPU
import GB.MBC
import GB.Logger
import GB.Utils

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC

data GB = GB CPU

newGB :: String -> IO GB
newGB file = do
  car <- readFileCartridge file
  mbc <- newMBC car
  cpu <- newCPU mbc
  pure $ GB cpu

stepGB :: GB -> IO ()
stepGB (GB cpu) = stepCPU cpu

serialToString :: GB -> IO String
serialToString (GB cpu) = (BC.unpack . B.pack) <$> readSerial cpu

setLogging :: GB -> Bool -> IO ()
setLogging (GB cpu) b = setIsLogging cpu b

readLog :: GB -> IO String
readLog (GB (CPU {..})) = do
  [log] <- readsLogger cpuLogger 1
  pure $ showCPULog log 
