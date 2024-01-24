module Main (main) where

import GB

import System.Environment
import Control.Monad

main :: IO ()
main = do
  [file] <- getArgs
  gb <- newGB file
  --setLogging gb True
  setLogging gb False
  loop gb 0

loop :: GB -> Int -> IO ()
loop gb n = do
  stepGB gb
  if n < 26000000 then
    loop gb (n + 1)
  else do
    ss <- serialToString gb
    putStrLn ss
    
  --when (n `mod` 1000000 == 0) $ do
    --l <- readLog gb
    --ss <- serialToString gb
    --putStrLn l
    --putStrLn ss
  --loop gb (n + 1)
