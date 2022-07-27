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
  n <- use $ logger.level
  when (n <= nn) $ do
    p <- use $ logger.isPrint
    when p $ do
      liftIO $ putStrLn str

    l <- use $ logger.isLogging
    when l $ do
      size <- use $ logger.bufSize
      buf <- use $ logger.buffer
      p <- use $ logger.pos
 
      liftIO $ VM.write buf p str
      if (p + 1) == size then
        logger.pos .= 0
      else
        logger.pos += 1

