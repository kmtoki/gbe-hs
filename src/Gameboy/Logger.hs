module Gameboy.Logger where

import Gameboy.Internal
import Gameboy.Utils

import qualified Data.Vector.Mutable as VM

newLoggerState :: Int -> Bool -> Bool -> Int -> IO LoggerState
newLoggerState n p l s = do
  buf <- VM.replicate s ""
  pure $ LoggerState {
    _level = n,
    _isPrint = p,
    _isLogging = l,
    _bufSize = s,
    _pos = 0,
    _buffer = buf
    }

logging :: Int -> String -> GB ()
logging nn str = do
  logger' <- use logger
  when ((logger'^.level) <= nn) $ do
    when (logger'^.isPrint) $ do
      liftIO $ putStrLn str

    when (logger'^.isLogging) $ do
      liftIO $ VM.write (logger'^.buffer) (logger'^.pos) str
      if ((logger'^.pos) + 1) == (logger'^.bufSize) then
        logger.pos .= 0
      else
        logger.pos += 1

