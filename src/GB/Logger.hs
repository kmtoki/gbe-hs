module GB.Logger (newLogger, writeLogger, readsLogger, readAllLogger) where

import GB.Prelude
import GB.Internal

import Data.Vector.Mutable qualified as V

data LoggerRegisters = Size | Pos
  deriving (Enum)

readReg :: Logger a -> LoggerRegisters -> IO Int
readReg (Logger {..}) r = do
  readStore regs $ fromEnum r

writeReg :: Logger a -> LoggerRegisters -> Int -> IO ()
writeReg (Logger {..}) r a = do
  writeStore regs (fromEnum r) a

newLogger :: Int -> a -> IO (Logger a)
newLogger size a = do 
  regs <- newStore 0xf 0
  buffer <- V.replicate size a
  let logger = Logger regs buffer
  writeReg logger Size size
  --writeReg logger IsLogging 1
  pure $ logger

writeLogger :: Logger a -> a -> IO ()
writeLogger logger@(Logger {..}) a = do
  pos <- readReg logger Pos
  size <- readReg logger Size
  let pos' = pos + 1
  writeReg logger Pos $ if (size - 1 == pos') then 0 else pos'
  V.write buffer pos' a

readsLogger :: Logger a -> Int -> IO [a]
readsLogger logger@(Logger {..}) n = do
  pos <- readReg logger Pos
  size <- readReg logger Size
  forM [0 .. n - 1] $ \i -> do
    let i' = if pos - i < 0 then size + (pos - i) else pos - i
    V.read buffer i'

readAllLogger :: Logger a -> IO [a]
readAllLogger logger@(Logger {..}) = do
  pos <- readReg logger Pos
  mapM (V.read buffer) [0 .. pos - 1]

