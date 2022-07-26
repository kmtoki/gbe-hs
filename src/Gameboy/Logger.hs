module Gameboy.Logger where

import Gameboy.Utils

import qualified Data.Vector.Mutable as VM


--newtype Logger m a = Logger {
--    runLogger :: StateT LoggerState m a
--  }
--  deriving (Functor, Applicative, Monad, MonadState LoggerState, MonadTrans, MonadIO)

type Logger a = StateT LoggerState IO a

data LoggerState = LoggerState {
    _level :: Int,
    _isLogging :: Bool,
    _isPrint :: Bool,
    _bufSize :: Int,
    _pos :: Int,
    _buffer :: VM.IOVector String
  }

makeLenses ''LoggerState


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

logging :: Int -> String -> Logger ()
logging nn str = do
  n <- use level
  when (n <= nn) $ do
    p <- use isPrint
    when p $ do
      liftIO $ putStrLn str

    l <- use isLogging
    when l $ do
      size <- use bufSize
      buf <- use buffer
      p <- use pos
 
      lift $ VM.write buf p str
      if (p + 1) == size then
        pos .= 0
      else
        pos += 1

