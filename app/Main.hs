module Main where

import Gameboy

import System.Environment
import Control.Monad


loop :: GameboyState -> Int -> IO ()
loop gb n = do
  gb' <- runGameboy gb
  when (n `mod` 1000000 == 0) $ putStrLn $ serialToString gb'
  if n < 26000000 then
    loop gb' (n + 1)
  else
    putStrLn $ serialToString gb'

main :: IO ()
main = do
  [rom] <- getArgs
  gb <- newGameboyState rom
  loop gb 1

