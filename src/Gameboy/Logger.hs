module Gameboy.Logger where

import Gameboy.Utils

import qualified Data.Vector as V


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
    _buffer :: V.Vector String
  }

makeLenses ''LoggerState


newLoggerState :: Int -> Bool -> Bool -> Int -> LoggerState
newLoggerState n p l s = LoggerState {
  _level = n,
  _isPrint = p,
  _isLogging = l,
  _bufSize = s,
  _buffer = V.empty
  }

logger :: Int -> String -> Logger ()
logger nn str = do
  p <- use isPrint
  n <- use level
  when (p && n <= nn) $ do
    liftIO $ putStrLn str

  l <- use isLogging
  s <- use bufSize
  b <- use buffer
  when l $ do
    buffer .=
      if V.length b < s then
        V.snoc b str
      else
        V.snoc (V.tail b) str
