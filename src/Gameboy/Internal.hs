module Gameboy.Internal where

import Gameboy.CPU
import Gameboy.Cartrige
import Gameboy.MBC
import Gameboy.Logger
import Gameboy.Utils

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

--type InternalMonad a = StateT Internal IO a
--
--data Internal = Internal {
--  mbc :: MBCState,
--  log :: LogState,
--  }
--
--type GBMonad a = StateT GBState IO a
--
----executeGB = do
----  cpu <~ (lift $ execStateT execCPU cpu')
--
--data GBState = GBState {
--  cpu :: CPUState,
--  mbc :: MBCState
--  }

--data GBState' = GBState' {
--    ram :: RAM,
--    ramx :: RAM,
--    rom :: ROM,
--    mbc :: MBC,
--    log :: Log,
--  }
--
--type GBState a = StateT GBState' IO a

--newtype GBStateM a = GBStateM (GBState -> IO (GBState, a))
--
--
--instance Functor GBStateM where
--  fmap f (GBStateM g) = GBStateM $ \s -> do
--    (s', a) <- g s
--    pure (s', f a)
--          
--
--instance Applicative GBStateM where
--  pure a = GBStateM (\s -> pure (s, a))
--  (GBStateM ff) <*> (GBStateM g) = GBStateM $ \s -> do
--    (s', a) <- g s
--    (s'', f) <- ff s'
--    pure (s'', f a)
--
--instance Monad GBStateM where
--  (GBStateM f) >>= g = GBStateM $ \s -> do
--    (s', a) <- f s
--    let (GBStateM h) = g a
--    h s'
--
--runGBStateM :: GBState -> GBStateM b -> IO GBState
--runGBStateM s (GBStateM f) = fst <$> f s
--
--read :: GBStateM a -> Word16 -> GBStateM Word8
--read m i = GBStateM $ \s -> do
--  (mbc', a) <- (reader s) (mbc s) i
--  pure $ (s { mbc = mbc' }, a)
--
--write :: GBStateM a -> Word16 -> GBStateM ()
--write m i a = GBStateM $ \s -> do
--  mbc' <- (writer s) (mbc s) i a
--  pure $ (s { mbc = mbc' }, ())



--class GBMemory mbc where
--  read :: GBState mbc -> Word16 -> IO Word8
--  write :: GBState mbc -> Word16 -> Word8 -> IO ()
--
--newtype MBCState = IORef


