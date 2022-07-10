module Gameboy.Logger where

import Gameboy.Utils

import qualified Data.Vector as V


--newtype Logger m a = Logger {
--    runLogger :: StateT LoggerState m a
--  }
--  deriving (Functor, Applicative, Monad, MonadState LoggerState, MonadTrans, MonadIO)

type Logger a = StateT LoggerState IO a

data LoggerState = LoggerState {
    _isLogging :: Bool,
    _isPrint :: Bool,
    _size :: Int,
    _buffer :: V.Vector String
  }

makeLenses ''LoggerState


newLoggerState :: Bool -> Bool -> Int -> LoggerState
newLoggerState l p s = LoggerState {
  _isLogging = l,
  _isPrint = p,
  _size = s,
  _buffer = V.empty
  }

logger :: String -> Logger ()
logger str = do
  p <- use isPrint
  when p $ liftIO $ putStrLn str

  l <- use isLogging
  s <- use size
  b <- use buffer
  when l $ do
    buffer .=
      if V.length b < s then
        V.snoc b str
      else
        V.snoc (V.tail b) str
