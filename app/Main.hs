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

  --when (n `mod` 1000000 == 0) $ do
  --  ss <- print $ serialToString gb
  --  putStrLn ss

  if n < 26000000 then
    loop gb (n + 1)
  else do
    ss <- serialToString gb
    putStrLn ss
