module Main (main) where

import GB
import GB.Prelude

import System.Environment
--import Control.Monad
import Control.Monad.Reader

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

  --when (n `mod` 1000000 == 0) $ do
  --  ss <- print $ serialToString gb
  --  putStrLn ss

  if n < 26000000 then
    loop (n + 1)
  else do
    ss <- serialToString
    liftIO $ putStrLn ss
