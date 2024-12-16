module GB.Internal (
  GB(..),
  GBState(..),
  CPU(..),
  MBC(..),
  Logger(..),
  Cartridge(..),
  Store(..),
  ROM,
  RAM,
  newStore,
  readStore,
  writeStore,
  getCPU,
  getMBC,
  ) where

import GB.Prelude
import GB.Internal.CPU (CPULog)
import GB.Internal.Cartridge

import Data.Vector.Mutable qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VM


newtype GB a = GB { unGB :: ReaderT GBState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader GBState)

data GBState = GBState {
  cpu :: CPU,
  mbc :: MBC
  }

newtype Store a = Store (VM.MVector (VM.PrimState IO) a)

type RAM = Store Word8
type ROM = VU.Vector Word8


data CPU = CPU { 
  cpuLogger :: Logger CPULog,
  serialLogger :: Logger Word8,
  joypadBuffer :: Store Word8,

  regs8 :: Store Word8,
  regs16 :: Store Word16,
  regs64 :: Store Word64
  }


data MBC = MBC {
  regs :: Store Word64,
  cartridge :: Cartridge,
  rom :: ROM,
  ram :: RAM,
  ramEx ::RAM
  --reader :: Word16 -> GB Word8,
  --writer :: Word16 -> Word8 -> GB ()
  }

data Logger a = Logger {
  regs :: Store Int,
  buffer :: V.IOVector a
  }

data Cartridge 
  = Cartridge {
    entryPoint :: ROM,
    logo :: ROM,
    title :: String,
    manufacturerCode :: ROM,
    cgbFlag :: CGBFlag,
    newLicenseeCode :: String,
    sgbFlag :: SGBFlag,
    mbcType :: MBCType,
    romSize :: Int,
    ramxSize :: Int,
    destinationCode :: DestinationCode,
    oldLicenseeCode :: String,
    maskROMVersionNumber :: Word8,
    headerChecksum :: Word8,
    globalChecksum :: Word16,
    raw :: ROM
  }

instance Show Cartridge where
  show (Cartridge { .. }) = 
    "Cartridge { " 
    ++ "entryPoint = " ++ show entryPoint 
    -- ++ ", logo = " ++ show logo 
    ++ ", logo = [...]"
    ++ ", title = " ++ (show $ filter (/= '\NUL') title)
    ++ ", manufacturerCode = " ++ show manufacturerCode 
    ++ ", cgbFlag = " ++ show cgbFlag 
    ++ ", newLicenseeCode = " ++ show newLicenseeCode 
    ++ ", sgbFlag = " ++ show sgbFlag 
    ++ ", mbcType = " ++ show mbcType 
    ++ ", romSize = " ++ show romSize 
    ++ ", ramxSize = " ++ show ramxSize 
    ++ ", destinationCode = " ++ show destinationCode 
    ++ ", oldLicenseeCode = " ++ show oldLicenseeCode 
    ++ ", maskROMVersionNumber = " ++ show maskROMVersionNumber 
    ++ ", headerChecksum = " ++ show headerChecksum 
    ++ ", globalChecksum = " ++ show globalChecksum 
    ++ ", raw = Cartridge[0x" ++ showHex (VU.length raw) ++ "]"
    ++ " }"
 

getCPU :: GB CPU
getCPU = (.cpu) <$> ask

getMBC :: GB MBC
getMBC = (.mbc) <$> ask

newStore :: VM.Unbox a => Int -> a -> IO (Store a)
newStore size a = do
  v <- VM.replicate size a
  pure $ Store v

readStore :: (MonadIO m, VM.Unbox a) => Store a -> Int -> m a
readStore (Store v) i = liftIO $ VM.read v i

writeStore :: (MonadIO m, VM.Unbox a) => Store a -> Int -> a -> m ()
writeStore (Store v) i w = liftIO $ VM.write v i w

modifyStore :: (MonadIO m, VM.Unbox a) => Store a -> Int -> (a -> a) -> m ()
modifyStore v i f = do
  a <- readStore v i
  writeStore v i $ f a
