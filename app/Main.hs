module Main where

import System.Environment
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Vector as V
import qualified Text.Read as T

import qualified Debugger as D
--import qualified Disassembler as DD

main :: IO ()
main = D.main'
