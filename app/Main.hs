module Main (main) where

import GB
import GB.Prelude

import System.Environment


msg :: String
msg = "\NULcpu_instrs\n\n01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok  \n\nPassed all tests"


main :: IO ()
main = do
  [file] <- getArgs
  gbs <- newGBState file
  flip runGB gbs $ do
    setLogging False
    loop 0
    

loop :: Int -> GB ()
loop n = do
  stepGB

  if n < 26000000 then
    loop (n + 1)
  else do
    ss <- serialToString
    liftIO $ putStrLn ss

