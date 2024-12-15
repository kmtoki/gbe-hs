module GB.CPU (
  GB(..),
  GBState(..),
  CPU(..),
  getCPU,
  newCPU, 
  stepCPU, 
  readSerial, 
  setIsLogging,
  showCPULog 
  --cpuLogger, 
  --serialLogger, 
  --joypadBuffer, 
  ) where

import Prelude hiding (log, cycle, and, or)
import Data.List (intersperse)

import GB.Logger 
import GB.MBC
import GB.Utils hiding (bit, xor)

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Builder qualified as B

newtype GB a = GB { runGB :: ReaderT GBState IO a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader GBState)

data GBState = GBState {
  cpu :: CPU
  }

data CPU = CPU { 
  mbc :: MBC,
  cpuLogger :: Logger CPULog,
  serialLogger :: Logger Word8,
  joypadBuffer :: Store Word8,

  regs8 :: Store Word8,
  regs16 :: Store Word16,
  regs64 :: Store Word64
  }


getCPU :: GB CPU
getCPU = cpu <$> ask


data CPUFlags = Carry | Half | Negative | Zero
  deriving (Enum, Show)

data CPURegisters8 = A | F | B | C | D | E | H | L | IME | Halt | Cycle | IsLogging
  deriving (Enum, Show, Eq)

data CPURegisters16 = SP | PC
  deriving (Enum, Show)

data CPURegisters64 = SysCounter | ExeCounter
  deriving (Enum, Show)

data Op8 
  = Reg8 CPURegisters8
  | A_
  | N
  | P_BC | P_DE | P_HL | P_NN_8
  | P_FF00_N | P_FF00_C
  | P_HL_INC
  | P_HL_DEC
  deriving (Show, Eq)

data Op16
  = Reg16 CPURegisters16
  | AF | BC | DE | HL
  | NN
  | P_NN_16
  deriving Show

data OpCond
  = IsZero | IsCarry | NotZero | NotCarry
  | Always
  deriving Show


data CPULog = CPULog { 
  a,f,b,c,d,e,h,l,halting,ime :: Word8,
  sp,pc :: Word16,
  exeCounter :: Word64,
  mbcROMBank :: Word64,
  regIF,regIE :: Word8,
  codes :: [Word8],
  stack :: [Word8],
  instruction :: String,
  op1, op2, op3 :: CPUOpLog
  }

data CPUOpLog 
  = Log8 Op8 | Log16 Op16 | LogCond OpCond 
  | LogInfo String
  | LogInfoW8 Word8 | LogInfoW16 Word16 | LogInfoI8 Int8 
  | None

log :: String -> CPUOpLog -> CPUOpLog -> CPUOpLog -> GB ()
log instruction op1 op2 op3 = do
  isLogging <- readReg8 IsLogging
  when (isLogging == 1) $ do
    CPU {..} <- getCPU
    [a,f,b,c,d,e,h,l,ime,halting] <- mapM readReg8 [A,F,B,C,D,E,H,L,IME,Halt]
    [sp,pc'] <- mapM readReg16 [SP,PC]
    let pc = pc' - 1
    codes <- mapM (liftIO . readMBC mbc . (+ pc)) [0..3]
    stack <- mapM (liftIO . readMBC mbc . (sp -)) [0..3]
    [exeCounter] <- mapM readReg64 [ExeCounter]
    [regIF,regIE] <- mapM (liftIO . readGBReg mbc) [IF,IE]
    mbcROMBank <- liftIO $ readMBCROMBank mbc
    liftIO $ writeLogger cpuLogger $ CPULog {..}

showCPULog :: CPULog -> String
showCPULog (CPULog {..}) = B.unpack $ B.toStrict $ B.toLazyByteString $
  "--- " <> B.word64Dec exeCounter <> "\n" <>
  "BANK:" <> B.word64Hex mbcROMBank <> "\n" <>
  "PC:" <> B.word16HexFixed pc <> " [" <> (mconcat $ intersperse " " $ map B.word8HexFixed codes) <>
  "] SP:" <> B.word16HexFixed sp <> " [" <> (mconcat $ intersperse " " $ map B.word8HexFixed stack) <>
  "]\nF:" <> (B.string8 $ if testBit f 7 then "Z" else "0")
        <> (B.string8 $ if testBit f 6 then "N" else "0")
        <> (B.string8 $ if testBit f 5 then "H" else "0")
        <> (B.string8 $ if testBit f 4 then "C" else "0") <>
  " A:" <> B.word8HexFixed a <>
  " BC:" <> B.word8HexFixed b <> B.word8HexFixed c <>
  " DE:" <> B.word8HexFixed d <> B.word8HexFixed e <>
  " HL:" <> B.word8HexFixed h <> B.word8HexFixed l <>
  " IME:" <> B.word8Dec ime <>
  " IF:" <> B.word8HexFixed regIF <>
  " IE:" <> B.word8HexFixed regIE <>
  " HALT:" <> B.word8Dec halting <> "\n" <>
  "> " <> B.string8 instruction <> " " <> showOpLog op1 <> " " <> showOpLog op2 <> 
  " " <> showOpLog op3 <> "\n"
  where
    showOpLog opLog = case opLog of
      Log8 (Reg8 r) -> B.string8 $ show r
      Log8 op -> B.string8 $ show op
      Log16 (Reg16 r) -> B.string8 $ show r
      Log16 op -> B.string8 $ show op
      LogCond op -> B.string8 $ show op
      LogInfo str -> B.string8 str
      LogInfoW8 w -> B.word8HexFixed w
      LogInfoW16 w -> B.word16HexFixed w
      LogInfoI8 i -> B.int8Dec i
      None -> ""



readReg8 :: CPURegisters8 -> GB Word8
readReg8 r = do
  CPU {..} <- getCPU
  liftIO $ readStore regs8 $ fromEnum r

readReg16 :: CPURegisters16 -> GB Word16
readReg16 r = do
  CPU {..} <- getCPU
  liftIO $ readStore regs16 $ fromEnum r

readReg64 :: CPURegisters64 -> GB Word64
readReg64 r = do
  CPU {..} <- getCPU
  liftIO $ readStore regs64 $ fromEnum r

writeReg8 :: CPURegisters8 -> Word8 -> GB ()
writeReg8 r n = do
  CPU {..} <- getCPU
  liftIO $ writeStore regs8 (fromEnum r) n

writeReg16 :: CPURegisters16 -> Word16 -> GB ()
writeReg16 r n = do
  CPU {..} <- getCPU
  liftIO $ writeStore regs16 (fromEnum r) n

writeReg64 :: CPURegisters64 -> Word64 -> GB ()
writeReg64 r n = do
  CPU {..} <- getCPU
  liftIO $ writeStore regs64 (fromEnum r) n

modifyReg8 :: CPURegisters8 -> (Word8 -> Word8) -> GB ()
modifyReg8 r f = readReg8 r >>= writeReg8 r . f

modifyReg16 :: CPURegisters16 -> (Word16 -> Word16) -> GB ()
modifyReg16 r f = readReg16 r >>= writeReg16 r . f

modifyReg64 :: CPURegisters64 -> (Word64 -> Word64) -> GB ()
modifyReg64 r f = readReg64 r >>= writeReg64 r . f


readFlag :: CPUFlags -> GB Bool
readFlag flag = do
  f <- readReg8 F
  pure $ testBit f $ 4 + fromEnum flag

writeFlag :: CPUFlags -> Bool -> GB ()
writeFlag flag bool = do
  f <- readReg8 F
  let f' = (if bool then setBit else clearBit) f (4 + fromEnum flag)
  writeReg8 F $ f' .&. 0b11110000


setIsLogging :: Bool -> GB ()
setIsLogging = writeReg8 IsLogging . fi . fromEnum


readSerial :: GB [Word8]
readSerial = do
  CPU {..} <- getCPU
  liftIO $ readAllLogger serialLogger


newCPU :: MBC -> IO CPU
newCPU mbc = do
  cpuLogger <- newLogger 0xffff (CPULog 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [0,0,0,0] [0,0,0,0] "" None None None)
  serialLogger <- newLogger 0xffff 0
  joypadBuffer <- newStore 1 0xff
  r8 <- newStore 0xff 0
  r16 <- newStore 0xff 0
  r64 <- newStore 0xff 0

  let cpu = CPU mbc cpuLogger serialLogger joypadBuffer r8 r16 r64

  --writeReg16 SP 0xfffe
  --writeReg16 PC 0x100
  pure cpu

tick :: GB ()
tick = do
  c <- readReg8 Cycle
  writeReg8 Cycle $ c + 1

fetch8 :: GB Word8
fetch8 = do
  CPU {..} <- getCPU
  pc <- readReg16 PC
  writeReg16 PC $ pc + 1
  tick
  liftIO $ readMBC mbc pc

fetch16 :: GB Word16
fetch16 = do
  l <- fetch8
  h <- fetch8
  pure $ toWord16 h l

push8 :: Word8 -> GB ()
push8 n = do
  CPU {..} <- getCPU
  sp <- subtract 1 <$> readReg16 SP
  writeReg16 SP sp
  liftIO $ writeMBC mbc sp n
  tick

push16 :: Word16 -> GB ()
push16 n = do
  let (h, l) = sepWord16 n
  push8 h
  push8 l

pop8 :: GB Word8
pop8 = do
  CPU {..} <- getCPU
  sp <- readReg16 SP
  n <- liftIO $ readMBC mbc sp
  modifyReg16 SP (+ 1)
  tick
  pure n

pop16 :: GB Word16
pop16 = do
  l <- pop8
  h <- pop8
  pure $ toWord16 h l

readOp8 :: Op8 -> GB Word8
readOp8 op = do
  CPU {..} <- getCPU
  case op of
    Reg8 r -> readReg8 r
    A_ -> readReg8 A
    N -> fetch8
    P_BC -> readOp16 BC >>= liftIO . readMBC mbc
    P_DE -> readOp16 DE >>= liftIO . readMBC mbc
    P_HL -> readOp16 HL >>= liftIO . readMBC mbc
    P_NN_8 -> fetch16 >>= liftIO . readMBC mbc
    P_FF00_N -> fetch8 >>= liftIO . readMBC mbc . (0xff00 +) . fi
    P_FF00_C -> readReg8 C >>= liftIO . readMBC mbc . (0xff00 +) . fi
    P_HL_INC -> do
      hl <- readOp16 HL
      writeOp16 HL $ hl + 1
      liftIO $ readMBC mbc hl
    P_HL_DEC -> do
      hl <- readOp16 HL
      writeOp16 HL $ hl - 1
      liftIO $ readMBC mbc hl

readOp16 :: Op16 -> GB Word16
readOp16 op = case op of
  Reg16 r -> readReg16 r
  AF -> toWord16 <$> readReg8 A <*> readReg8 F
  BC -> toWord16 <$> readReg8 B <*> readReg8 C
  DE -> toWord16 <$> readReg8 D <*> readReg8 E
  HL -> toWord16 <$> readReg8 H <*> readReg8 L
  NN -> fetch16
  _ -> error $ "CPU.readOp16 unexpect " ++ show op

writeOp8 :: Op8 -> Word8 -> GB ()
writeOp8 op n = do
  CPU {..} <- getCPU
  case op of
    Reg8 r -> writeReg8 r n
    A_ -> writeReg8 A n
    P_BC -> readOp16 BC >>= \a -> liftIO $ writeMBC mbc a n
    P_DE -> readOp16 DE >>= \a -> liftIO $ writeMBC mbc a n
    P_HL -> readOp16 HL >>= \a -> liftIO $ writeMBC mbc a n
    P_NN_8 -> fetch16 >>= \a -> liftIO $ writeMBC mbc a n
    P_FF00_C -> readReg8 C >>= \c -> liftIO $ writeMBC mbc (0xff00 + fi c) n
    P_FF00_N -> fetch8 >>= \a -> liftIO $ writeMBC mbc (0xff00 + fi a) n
    P_HL_INC -> do
      hl <- readOp16 HL
      writeOp16 HL $ hl + 1
      liftIO $ writeMBC mbc hl n
    P_HL_DEC -> do
      hl <- readOp16 HL
      writeOp16 HL $ hl - 1
      liftIO $ writeMBC mbc hl n
    _ -> error $ "CPU.writeOp8 unexpect " ++ show op

writeOp16 :: Op16 -> Word16 -> GB ()
writeOp16 op n =
  let (h,l) = sepWord16 n in 
    case op of
      Reg16 r -> writeReg16 r n
      AF -> writeReg8 A h >> writeReg8 F (l .&. 0b11110000)
      BC -> writeReg8 B h >> writeReg8 C l
      DE -> writeReg8 D h >> writeReg8 E l
      HL -> writeReg8 H h >> writeReg8 L l
      P_NN_16 -> do
        CPU {..} <- getCPU
        a <- fetch16
        liftIO $ writeMBC mbc a l
        liftIO $ writeMBC mbc (a + 1) h
      _ -> error $ "CPU.writeOp16 unexpect " ++ show op

condFlag :: OpCond -> GB Bool
condFlag op = case op of
  IsZero -> readFlag Zero
  NotZero -> not <$> readFlag Zero
  IsCarry -> readFlag Carry
  NotCarry -> not <$> readFlag Carry
  Always -> pure True


ld8 :: Op8 -> Op8 -> GB ()
ld8 op1 op2 = do
  log "LD" (Log8 op1) (Log8 op2) None
  n <- readOp8 op2
  writeOp8 op1 n

ld16 :: Op16 -> Op16 -> GB ()
ld16 op1 op2 = do
  log "LD" (Log16 op1) (Log16 op2) None
  n <- readOp16 op2
  writeOp16 op1 n

ld16_hl_sp_n :: GB ()
ld16_hl_sp_n = do
  n <- fetch8

  modifyReg16 PC (subtract 1)
  log "LD" (Log16 HL) (Log16 $ Reg16 SP) (LogInfoI8 $ fi n)
  modifyReg16 PC (+ 1)

  sp <- readReg16 SP
  let (a, carry, half) = addCarryHalf sp (fi n :: Int8)

  writeOp16 HL a
  writeFlag Carry carry
  writeFlag Half half
  writeFlag Negative False
  writeFlag Zero False
  tick

push :: Op16 -> GB ()
push op = do
  log "PUSH" (Log16 op) None None
  n <- readOp16 op
  tick
  push16 n
  
pop :: Op16 -> GB ()
pop op = do
  log "POP" (Log16 op) None None
  n <- pop16
  writeOp16 op n

add :: Op8 -> GB ()
add op = do
  log "ADD" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op
  let (a', carry, half) = addCarryHalf a n
  writeReg8 A a'
  writeFlag Carry carry
  writeFlag Half half
  writeFlag Negative False
  writeFlag Zero $ a' == 0

adc :: Op8 -> GB ()
adc op = do
  log "ADC" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op
  c <- readFlag Carry
  let (a', carry, half) = addCarryHalf a n
  let (a'', carry', half') = addCarryHalf a' (fi $ fromEnum c :: Word8)
  writeReg8 A a''
  writeFlag Carry $ carry || carry'
  writeFlag Half $ half || half'
  writeFlag Negative False
  writeFlag Zero $ a'' == 0

sub :: Op8 -> GB ()
sub op = do
  log "SUB" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op
  let (a', carry, half) = subCarryHalf a n
  writeReg8 A a'
  writeFlag Carry carry
  writeFlag Half half
  writeFlag Negative True
  writeFlag Zero $ a' == 0

sbc :: Op8 -> GB ()
sbc op = do
  log "SBC" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op
  c <- readFlag Carry
  let (a', carry, half) = subCarryHalf a n
  let (a'', carry', half') = subCarryHalf a' (fi $ fromEnum c :: Word8)
  writeReg8 A a''
  writeFlag Carry $ carry || carry'
  writeFlag Half $ half || half'
  writeFlag Negative True
  writeFlag Zero $ a'' == 0

and :: Op8 -> GB ()
and op = do
  log "AND" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op 
  let a' = a .&. n
  writeReg8 A a'
  writeFlag Carry False
  writeFlag Half True
  writeFlag Negative False
  writeFlag Zero $ a' == 0

or :: Op8 -> GB ()
or op = do
  log "OR" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op 
  let a' = a .|. n
  writeReg8 A a'
  writeFlag Carry False
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ a' == 0

xor :: Op8 -> GB ()
xor op = do
  log "XOR" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op 
  let a' = a .^. n
  writeReg8 A a'
  writeFlag Carry False
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ a' == 0

cp :: Op8 -> GB ()
cp op = do
  log "CP" (Log8 op) None None
  a <- readReg8 A
  n <- readOp8 op 
  let (a',carry,half) = subCarryHalf a n
  writeFlag Carry carry
  writeFlag Half half
  writeFlag Negative True
  writeFlag Zero $ a' == 0

inc8 :: Op8 -> GB ()
inc8 op = do
  log "INC" (Log8 op) None None
  a <- readOp8 op
  let (a',_,half) = addCarryHalf a (1 :: Word8)
  writeOp8 op a'
  writeFlag Half half
  writeFlag Negative False
  writeFlag Zero $ a' == 0

dec8 :: Op8 -> GB ()
dec8 op = do
  log "DEC" (Log8 op) None None
  a <- readOp8 op
  let (a',_,half) = subCarryHalf a (1 :: Word8)
  writeOp8 op a'
  writeFlag Half half
  writeFlag Negative True
  writeFlag Zero $ a' == 0

add_hl :: Op16 -> GB ()
add_hl op = do
  log "DEC" (Log16 HL) (Log16 op) None
  hl <- readOp16 HL
  n <- readOp16 op
  let (hl',carry,half) = addCarryHalf hl n
  writeOp16 HL hl'
  writeFlag Carry carry
  writeFlag Half half
  writeFlag Negative False

add_sp_n :: GB ()
add_sp_n = do
  n <- fetch8
  sp <- readReg16 SP

  modifyReg16 PC (subtract 1)
  log "ADD" (Log16 $ Reg16 SP) (Log8 N) (LogInfoI8 $ fi n)
  modifyReg16 PC (+ 1)

  let (sp',carry,half) = addCarryHalf sp (fi n :: Int8)
  writeReg16 SP sp'
  writeFlag Carry carry
  writeFlag Half half
  writeFlag Negative False
  writeFlag Zero False
  tick
  tick

inc16 :: Op16 -> GB ()
inc16 op = do
  log "INC" (Log16 op) None None
  a <- readOp16 op
  let (a',_,_) = addCarryHalf a (1 :: Word16)
  writeOp16 op a'

dec16 :: Op16 -> GB ()
dec16 op = do
  log "DEC" (Log16 op) None None
  a <- readOp16 op
  let (a',_,_) = subCarryHalf a (1 :: Word16)
  writeOp16 op a'

daa :: GB ()
daa = do
  log "DAA" None None None
  a <- readReg8 A
  carry <- readFlag Carry
  half <- readFlag Half
  negative <- readFlag Negative

  let 
    adjust = (if carry then 0x60 else 0) .|. (if half then 0x06 else 0)
    adjust' = 
      if not negative then
        adjust .|. (if a .&. 0x0f > 0x09 then 0x06 else 0) .|. (if a > 0x99 then 0x60 else 0)
      else
        adjust
    a' = if not negative then a + adjust' else a - adjust'

  writeReg8 A a'
  writeFlag Carry $ adjust' >= 0x60
  writeFlag Half False 
  writeFlag Zero $ a' == 0

cpl :: GB ()
cpl = do
  log "CPL" None None None
  modifyReg8 A (.^. 0xff)
  writeFlag Half True
  writeFlag Negative True

ccf :: GB ()
ccf = do
  log "CCF" None None None
  carry <- readFlag Carry
  writeFlag Carry $ not carry
  writeFlag Half False
  writeFlag Negative False

scf :: GB ()
scf = do
  log "SCF" None None None
  writeFlag Carry True
  writeFlag Half False
  writeFlag Negative False

di :: GB ()
di = do
  log "DI" None None None
  writeReg8 IME 0

ei :: GB ()
ei = do
  log "EI" None None None
  writeReg8 IME 1

halt :: GB ()
halt = do
  log "HALT" None None None
  writeReg8 Halt 1

stop :: GB ()
stop = do
  log "STOP" None None None
  --writeReg8 Halt 1

nop :: GB ()
nop = do
  log "NOP" None None None
  tick

jp :: OpCond -> GB ()
jp op = do
  nn <- fetch16

  modifyReg16 PC (subtract 2)
  log "JP" (LogCond op) None $ LogInfoW16 nn
  modifyReg16 PC (+ 2)

  bool <- condFlag op
  when bool $ do
    writeReg16 PC nn
    tick

jp_p_hl :: GB ()
jp_p_hl = do
  hl <- readOp16 HL 
  log "JP" (Log16 HL) None (LogInfoW16 hl)
  writeReg16 PC hl
  tick


jr :: OpCond -> GB ()
jr op = do
  n <- fetch8

  modifyReg16 PC (subtract 1) 
  log "JR" (LogCond op) None (LogInfoI8 $ fi n)
  modifyReg16 PC (+ 1)

  bool <- condFlag op
  when bool $ do
    pc <- readReg16 PC
    let (pc',_,_) = addCarryHalf pc (fi n :: Int8)
    writeReg16 PC pc'
    tick

call :: OpCond -> GB ()
call op = do
  nn <- fetch16

  modifyReg16 PC (subtract 2)
  log "CALL" (LogCond op) None $ LogInfoW16 nn
  modifyReg16 PC (+ 2)

  bool <- condFlag op
  when bool $ do
    tick
    pc <- readReg16 PC
    push16 pc
    writeReg16 PC nn

ret :: OpCond -> GB ()
ret op = do
  modifyReg16 SP (+ 1)
  log "RET" (LogCond op) None None
  modifyReg16 SP (subtract 1)

  bool <- condFlag op
  when bool $ do
    pc <- pop16
    writeReg16 PC pc
    tick

reti :: GB ()
reti = do
  pc <- pop16

  modifyReg16 SP (subtract 2)
  log "RETI" None None $ LogInfoW16 pc
  modifyReg16 SP (+ 2)

  writeReg16 PC pc
  tick
  writeReg8 IME 1

rst :: Word16 -> GB ()
rst nn = do
  log "RST" None None $ LogInfoW16 nn
  tick
  pc <- readReg16 PC
  push16 pc
  writeReg16 PC nn

swap :: Op8 -> GB ()
swap op = do
  log "SWAP" (Log8 op) None None
  r <- readOp8 op
  let a = (r `shiftL` 4) .|. (r `shiftR` 4)
  writeOp8 op a
  writeFlag Carry False
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ a == 0

rlc :: Op8 -> GB ()
rlc op = do
  log "RLC" (Log8 op) None None
  r <- readOp8 op
  let 
    c = r `shiftR` 7
    a = (r `shiftL` 1) .|. c
  writeOp8 op a
  writeFlag Carry $ c == 1
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ if op == A_ then False else a == 0

rl :: Op8 -> GB ()
rl op = do
  log "RL" (Log8 op) None None
  r <- readOp8 op
  carry <- readFlag Carry
  let a = (r `shiftL` 1) .|. (toNum carry)
  writeOp8 op a
  writeFlag Carry $ r `shiftR` 7 == 1
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ if op == A_ then False else a == 0

rrc :: Op8 -> GB ()
rrc op = do
  log "RRC" (Log8 op) None None
  r <- readOp8 op
  let 
    c = r .&. 1
    a = (r `shiftL` 7) .|. (r `shiftR` 1)
  writeOp8 op a
  writeFlag Carry $ c == 1
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ if op == A_ then False else a == 0

rr :: Op8 -> GB ()
rr op = do
  log "RR" (Log8 op) None None
  r <- readOp8 op
  carry <- readFlag Carry
  let a = ((toNum carry :: Word8) `shiftL` 7) .|. (r `shiftR` 1)
  writeOp8 op a
  writeFlag Carry $ r .&. 1 == 1
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ if op == A_ then False else a == 0

sla :: Op8 -> GB ()
sla op = do
  log "SLA" (Log8 op) None None
  r <- readOp8 op
  let a = r `shiftL` 1
  writeOp8 op a
  writeFlag Carry $ r `shiftR` 7 == 1
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ a == 0

sra :: Op8 -> GB ()
sra op = do
  log "SRA" (Log8 op) None None
  r <- readOp8 op
  let a = (r .&. 0b10000000) .|. (r `shiftR` 1)
  writeOp8 op a
  writeFlag Carry $ r .&. 1 == 1
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ a == 0

srl :: Op8 -> GB ()
srl op = do
  log "SRL" (Log8 op) None None
  r <- readOp8 op
  let a = r `shiftR` 1
  writeOp8 op a
  writeFlag Carry $ r .&. 1 == 1
  writeFlag Half False
  writeFlag Negative False
  writeFlag Zero $ a == 0

bit :: Word8 -> Op8 -> GB ()
bit n op = do
  log "BIT" (Log8 op) (LogInfoW8 n) None
  r <- readOp8 op
  let a = testBit r $ fi n
  writeFlag Half True
  writeFlag Negative False
  writeFlag Zero $ a == False 

set :: Word8 -> Op8 -> GB ()
set n op = do
  log "SET" (Log8 op) (LogInfoW8 n) None
  r <- readOp8 op
  let a = setBit r $ fi n
  writeOp8 op a

res :: Word8 -> Op8 -> GB ()
res n op = do
  log "RET" (Log8 op) (LogInfoW8 n) None
  r <- readOp8 op
  let a = clearBit r $ fi n
  writeOp8 op a


executeInstruction :: GB ()
executeInstruction = do
  code <- fetch8
  case code of
    0x3e -> ld8 (Reg8 A) N
    0x06 -> ld8 (Reg8 B) N
    0x0e -> ld8 (Reg8 C) N
    0x16 -> ld8 (Reg8 D) N
    0x1e -> ld8 (Reg8 E) N
    0x26 -> ld8 (Reg8 H) N
    0x2e -> ld8 (Reg8 L) N
    0x7f -> ld8 (Reg8 A) (Reg8 A)
    0x78 -> ld8 (Reg8 A) (Reg8 B)
    0x79 -> ld8 (Reg8 A) (Reg8 C)
    0x7a -> ld8 (Reg8 A) (Reg8 D)
    0x7b -> ld8 (Reg8 A) (Reg8 E)
    0x7c -> ld8 (Reg8 A) (Reg8 H)
    0x7d -> ld8 (Reg8 A) (Reg8 L)
    0x7e -> ld8 (Reg8 A) P_HL
    0x0a -> ld8 (Reg8 A) P_BC
    0x1a -> ld8 (Reg8 A) P_DE
    0x47 -> ld8 (Reg8 B) (Reg8 A)
    0x40 -> ld8 (Reg8 B) (Reg8 B)
    0x41 -> ld8 (Reg8 B) (Reg8 C)
    0x42 -> ld8 (Reg8 B) (Reg8 D)
    0x43 -> ld8 (Reg8 B) (Reg8 E)
    0x44 -> ld8 (Reg8 B) (Reg8 H)
    0x45 -> ld8 (Reg8 B) (Reg8 L)
    0x46 -> ld8 (Reg8 B) P_HL
    0x4f -> ld8 (Reg8 C) (Reg8 A)
    0x48 -> ld8 (Reg8 C) (Reg8 B)
    0x49 -> ld8 (Reg8 C) (Reg8 C)
    0x4a -> ld8 (Reg8 C) (Reg8 D)
    0x4b -> ld8 (Reg8 C) (Reg8 E)
    0x4c -> ld8 (Reg8 C) (Reg8 H)
    0x4d -> ld8 (Reg8 C) (Reg8 L)
    0x4e -> ld8 (Reg8 C) P_HL
    0x57 -> ld8 (Reg8 D) (Reg8 A)
    0x50 -> ld8 (Reg8 D) (Reg8 B)
    0x51 -> ld8 (Reg8 D) (Reg8 C)
    0x52 -> ld8 (Reg8 D) (Reg8 D)
    0x53 -> ld8 (Reg8 D) (Reg8 E)
    0x54 -> ld8 (Reg8 D) (Reg8 H)
    0x55 -> ld8 (Reg8 D) (Reg8 L)
    0x56 -> ld8 (Reg8 D) P_HL
    0x5f -> ld8 (Reg8 E) (Reg8 A)
    0x58 -> ld8 (Reg8 E) (Reg8 B)
    0x59 -> ld8 (Reg8 E) (Reg8 C)
    0x5a -> ld8 (Reg8 E) (Reg8 D)
    0x5b -> ld8 (Reg8 E) (Reg8 E)
    0x5c -> ld8 (Reg8 E) (Reg8 H)
    0x5d -> ld8 (Reg8 E) (Reg8 L)
    0x5e -> ld8 (Reg8 E) P_HL
    0x67 -> ld8 (Reg8 H) (Reg8 A)
    0x60 -> ld8 (Reg8 H) (Reg8 B)
    0x61 -> ld8 (Reg8 H) (Reg8 C)
    0x62 -> ld8 (Reg8 H) (Reg8 D)
    0x63 -> ld8 (Reg8 H) (Reg8 E)
    0x64 -> ld8 (Reg8 H) (Reg8 H)
    0x65 -> ld8 (Reg8 H) (Reg8 L)
    0x66 -> ld8 (Reg8 H) P_HL
    0x6f -> ld8 (Reg8 L) (Reg8 A)
    0x68 -> ld8 (Reg8 L) (Reg8 B)
    0x69 -> ld8 (Reg8 L) (Reg8 C)
    0x6a -> ld8 (Reg8 L) (Reg8 D)
    0x6b -> ld8 (Reg8 L) (Reg8 E)
    0x6c -> ld8 (Reg8 L) (Reg8 H)
    0x6d -> ld8 (Reg8 L) (Reg8 L)
    0x6e -> ld8 (Reg8 L) P_HL

    0x70 -> ld8 P_HL (Reg8 B)
    0x71 -> ld8 P_HL (Reg8 C)
    0x72 -> ld8 P_HL (Reg8 D)
    0x73 -> ld8 P_HL (Reg8 E)
    0x74 -> ld8 P_HL (Reg8 H)
    0x75 -> ld8 P_HL (Reg8 L)
    0x36 -> ld8 P_HL N
    0x02 -> ld8 P_BC (Reg8 A)
    0x12 -> ld8 P_DE (Reg8 A)
    0x77 -> ld8 P_HL (Reg8 A)
    0xea -> ld8 P_NN_8 (Reg8 A)

    0xf0 -> ld8 (Reg8 A) P_FF00_N
    0xf2 -> ld8 (Reg8 A) P_FF00_C
    0xfa -> ld8 (Reg8 A) P_NN_8
    0xe0 -> ld8 P_FF00_N (Reg8 A)
    0xe2 -> ld8 P_FF00_C (Reg8 A)

    0x22 -> ld8 P_HL_INC (Reg8 A)
    0x2a -> ld8 (Reg8 A) P_HL_INC
    0x32 -> ld8 P_HL_DEC (Reg8 A)
    0x3a -> ld8 (Reg8 A) P_HL_DEC

    0x01 -> ld16 BC NN
    0x11 -> ld16 DE NN
    0x21 -> ld16 HL NN
    0x31 -> ld16 (Reg16 SP) NN
    0xf9 -> ld16 (Reg16 SP) HL
    0x08 -> ld16 P_NN_16 (Reg16 SP)
    0xf8 -> ld16_hl_sp_n 

    0xf5 -> push AF
    0xc5 -> push BC
    0xd5 -> push DE
    0xe5 -> push HL
    0xf1 -> pop AF
    0xc1 -> pop BC
    0xd1 -> pop DE
    0xe1 -> pop HL

    0x87 -> add (Reg8 A)
    0x80 -> add (Reg8 B)
    0x81 -> add (Reg8 C)
    0x82 -> add (Reg8 D)
    0x83 -> add (Reg8 E)
    0x84 -> add (Reg8 H)
    0x85 -> add (Reg8 L)
    0x86 -> add P_HL
    0xc6 -> add N

    0x8f -> adc (Reg8 A)
    0x88 -> adc (Reg8 B)
    0x89 -> adc (Reg8 C)
    0x8a -> adc (Reg8 D)
    0x8b -> adc (Reg8 E)
    0x8c -> adc (Reg8 H)
    0x8d -> adc (Reg8 L)
    0x8e -> adc P_HL
    0xce -> adc N

    0x97 -> sub (Reg8 A)
    0x90 -> sub (Reg8 B)
    0x91 -> sub (Reg8 C)
    0x92 -> sub (Reg8 D)
    0x93 -> sub (Reg8 E)
    0x94 -> sub (Reg8 H)
    0x95 -> sub (Reg8 L)
    0x96 -> sub P_HL
    0xd6 -> sub N

    0x9f -> sbc (Reg8 A)
    0x98 -> sbc (Reg8 B)
    0x99 -> sbc (Reg8 C)
    0x9a -> sbc (Reg8 D)
    0x9b -> sbc (Reg8 E)
    0x9c -> sbc (Reg8 H)
    0x9d -> sbc (Reg8 L)
    0x9e -> sbc P_HL
    0xde -> sbc N

    0xa7 -> and (Reg8 A)
    0xa0 -> and (Reg8 B)
    0xa1 -> and (Reg8 C)
    0xa2 -> and (Reg8 D)
    0xa3 -> and (Reg8 E)
    0xa4 -> and (Reg8 H)
    0xa5 -> and (Reg8 L)
    0xa6 -> and P_HL
    0xe6 -> and N

    0xb7 -> or (Reg8 A)
    0xb0 -> or (Reg8 B)
    0xb1 -> or (Reg8 C)
    0xb2 -> or (Reg8 D)
    0xb3 -> or (Reg8 E)
    0xb4 -> or (Reg8 H)
    0xb5 -> or (Reg8 L)
    0xb6 -> or P_HL
    0xf6 -> or N

    0xaf -> xor (Reg8 A)
    0xa8 -> xor (Reg8 B)
    0xa9 -> xor (Reg8 C)
    0xaa -> xor (Reg8 D)
    0xab -> xor (Reg8 E)
    0xac -> xor (Reg8 H)
    0xad -> xor (Reg8 L)
    0xae -> xor P_HL
    0xee -> xor N

    0xbf -> cp (Reg8 A)
    0xb8 -> cp (Reg8 B)
    0xb9 -> cp (Reg8 C)
    0xba -> cp (Reg8 D)
    0xbb -> cp (Reg8 E)
    0xbc -> cp (Reg8 H)
    0xbd -> cp (Reg8 L)
    0xbe -> cp P_HL
    0xfe -> cp N

    0x3c -> inc8 (Reg8 A)
    0x04 -> inc8 (Reg8 B)
    0x0c -> inc8 (Reg8 C)
    0x14 -> inc8 (Reg8 D)
    0x1c -> inc8 (Reg8 E)
    0x24 -> inc8 (Reg8 H)
    0x2c -> inc8 (Reg8 L)
    0x34 -> inc8 P_HL

    0x3d -> dec8 (Reg8 A)
    0x05 -> dec8 (Reg8 B)
    0x0d -> dec8 (Reg8 C)
    0x15 -> dec8 (Reg8 D)
    0x1d -> dec8 (Reg8 E)
    0x25 -> dec8 (Reg8 H)
    0x2d -> dec8 (Reg8 L)
    0x35 -> dec8 P_HL

    0x09 -> add_hl BC
    0x19 -> add_hl DE
    0x29 -> add_hl HL
    0x39 -> add_hl (Reg16 SP)
    0xe8 -> add_sp_n 

    0x03 -> inc16 BC
    0x13 -> inc16 DE
    0x23 -> inc16 HL
    0x33 -> inc16 (Reg16 SP)

    0x0b -> dec16 BC
    0x1b -> dec16 DE
    0x2b -> dec16 HL
    0x3b -> dec16 (Reg16 SP)

    0x07 -> rlc A_
    0x17 -> rl A_
    0x0f -> rrc A_
    0x1f -> rr A_

    0x27 -> daa 
    0x2f -> cpl 
    0x3f -> ccf 
    0x37 -> scf 
    0xf3 -> di 
    0xfb -> ei 
    0x76 -> halt 
    0x00 -> nop 

    0xc3 -> jp Always
    0xc2 -> jp NotZero
    0xca -> jp IsZero
    0xd2 -> jp NotCarry
    0xda -> jp IsCarry
    0xe9 -> jp_p_hl 
    0x18 -> jr Always
    0x20 -> jr NotZero
    0x28 -> jr IsZero
    0x30 -> jr NotCarry
    0x38 -> jr IsCarry
    0xcd -> call Always
    0xc4 -> call NotZero
    0xcc -> call IsZero
    0xd4 -> call NotCarry
    0xdc -> call IsCarry
    0xc7 -> rst 0x00
    0xcf -> rst 0x08
    0xd7 -> rst 0x10
    0xdf -> rst 0x18
    0xe7 -> rst 0x20
    0xef -> rst 0x28
    0xf7 -> rst 0x30
    0xff -> rst 0x38
    0xc9 -> ret Always
    0xc0 -> ret NotZero
    0xc8 -> ret IsZero
    0xd0 -> ret NotCarry
    0xd8 -> ret IsCarry
    0xd9 -> reti 

    0x10 -> do
      code10 <- fetch8
      case code10 of
        0x00 -> stop 
        _ -> error $ "CPU.execute: undefined instruction 0x10 " ++ showHex code10

    0xcb -> do
      codeCB <- fetch8
      case codeCB of
        0x37 -> swap (Reg8 A)
        0x30 -> swap (Reg8 B)
        0x31 -> swap (Reg8 C)
        0x32 -> swap (Reg8 D)
        0x33 -> swap (Reg8 E)
        0x34 -> swap (Reg8 H)
        0x35 -> swap (Reg8 L)
        0x36 -> swap P_HL

        0x07 -> rlc (Reg8 A)
        0x00 -> rlc (Reg8 B)
        0x01 -> rlc (Reg8 C)
        0x02 -> rlc (Reg8 D)
        0x03 -> rlc (Reg8 E)
        0x04 -> rlc (Reg8 H)
        0x05 -> rlc (Reg8 L)
        0x06 -> rlc P_HL

        0x17 -> rl (Reg8 A)
        0x10 -> rl (Reg8 B)
        0x11 -> rl (Reg8 C)
        0x12 -> rl (Reg8 D)
        0x13 -> rl (Reg8 E)
        0x14 -> rl (Reg8 H)
        0x15 -> rl (Reg8 L)
        0x16 -> rl P_HL

        0x0f -> rrc (Reg8 A)
        0x08 -> rrc (Reg8 B)
        0x09 -> rrc (Reg8 C)
        0x0a -> rrc (Reg8 D)
        0x0b -> rrc (Reg8 E)
        0x0c -> rrc (Reg8 H)
        0x0d -> rrc (Reg8 L)
        0x0e -> rrc P_HL

        0x1f -> rr (Reg8 A)
        0x18 -> rr (Reg8 B)
        0x19 -> rr (Reg8 C)
        0x1a -> rr (Reg8 D)
        0x1b -> rr (Reg8 E)
        0x1c -> rr (Reg8 H)
        0x1d -> rr (Reg8 L)
        0x1e -> rr P_HL

        0x27 -> sla (Reg8 A)
        0x20 -> sla (Reg8 B)
        0x21 -> sla (Reg8 C)
        0x22 -> sla (Reg8 D)
        0x23 -> sla (Reg8 E)
        0x24 -> sla (Reg8 H)
        0x25 -> sla (Reg8 L)
        0x26 -> sla P_HL

        0x2f -> sra (Reg8 A)
        0x28 -> sra (Reg8 B)
        0x29 -> sra (Reg8 C)
        0x2a -> sra (Reg8 D)
        0x2b -> sra (Reg8 E)
        0x2c -> sra (Reg8 H)
        0x2d -> sra (Reg8 L)
        0x2e -> sra P_HL

        0x3f -> srl (Reg8 A)
        0x38 -> srl (Reg8 B)
        0x39 -> srl (Reg8 C)
        0x3a -> srl (Reg8 D)
        0x3b -> srl (Reg8 E)
        0x3c -> srl (Reg8 H)
        0x3d -> srl (Reg8 L)
        0x3e -> srl P_HL

        0x47 -> bit 0 (Reg8 A)
        0x40 -> bit 0 (Reg8 B)
        0x41 -> bit 0 (Reg8 C)
        0x42 -> bit 0 (Reg8 D)
        0x43 -> bit 0 (Reg8 E)
        0x44 -> bit 0 (Reg8 H)
        0x45 -> bit 0 (Reg8 L)
        0x46 -> bit 0 P_HL
        0x4f -> bit 1 (Reg8 A)
        0x48 -> bit 1 (Reg8 B)
        0x49 -> bit 1 (Reg8 C)
        0x4a -> bit 1 (Reg8 D)
        0x4b -> bit 1 (Reg8 E)
        0x4c -> bit 1 (Reg8 H)
        0x4d -> bit 1 (Reg8 L)
        0x4e -> bit 1 P_HL
        0x57 -> bit 2 (Reg8 A)
        0x50 -> bit 2 (Reg8 B)
        0x51 -> bit 2 (Reg8 C)
        0x52 -> bit 2 (Reg8 D)
        0x53 -> bit 2 (Reg8 E)
        0x54 -> bit 2 (Reg8 H)
        0x55 -> bit 2 (Reg8 L)
        0x56 -> bit 2 P_HL
        0x5f -> bit 3 (Reg8 A)
        0x58 -> bit 3 (Reg8 B)
        0x59 -> bit 3 (Reg8 C)
        0x5a -> bit 3 (Reg8 D)
        0x5b -> bit 3 (Reg8 E)
        0x5c -> bit 3 (Reg8 H)
        0x5d -> bit 3 (Reg8 L)
        0x5e -> bit 3 P_HL
        0x67 -> bit 4 (Reg8 A)
        0x60 -> bit 4 (Reg8 B)
        0x61 -> bit 4 (Reg8 C)
        0x62 -> bit 4 (Reg8 D)
        0x63 -> bit 4 (Reg8 E)
        0x64 -> bit 4 (Reg8 H)
        0x65 -> bit 4 (Reg8 L)
        0x66 -> bit 4 P_HL
        0x6f -> bit 5 (Reg8 A)
        0x68 -> bit 5 (Reg8 B)
        0x69 -> bit 5 (Reg8 C)
        0x6a -> bit 5 (Reg8 D)
        0x6b -> bit 5 (Reg8 E)
        0x6c -> bit 5 (Reg8 H)
        0x6d -> bit 5 (Reg8 L)
        0x6e -> bit 5 P_HL
        0x77 -> bit 6 (Reg8 A)
        0x70 -> bit 6 (Reg8 B)
        0x71 -> bit 6 (Reg8 C)
        0x72 -> bit 6 (Reg8 D)
        0x73 -> bit 6 (Reg8 E)
        0x74 -> bit 6 (Reg8 H)
        0x75 -> bit 6 (Reg8 L)
        0x76 -> bit 6 P_HL
        0x7f -> bit 7 (Reg8 A)
        0x78 -> bit 7 (Reg8 B)
        0x79 -> bit 7 (Reg8 C)
        0x7a -> bit 7 (Reg8 D)
        0x7b -> bit 7 (Reg8 E)
        0x7c -> bit 7 (Reg8 H)
        0x7d -> bit 7 (Reg8 L)
        0x7e -> bit 7 P_HL

        0xc7 -> set 0 (Reg8 A)
        0xc0 -> set 0 (Reg8 B)
        0xc1 -> set 0 (Reg8 C)
        0xc2 -> set 0 (Reg8 D)
        0xc3 -> set 0 (Reg8 E)
        0xc4 -> set 0 (Reg8 H)
        0xc5 -> set 0 (Reg8 L)
        0xc6 -> set 0 P_HL
        0xcf -> set 1 (Reg8 A)
        0xc8 -> set 1 (Reg8 B)
        0xc9 -> set 1 (Reg8 C)
        0xca -> set 1 (Reg8 D)
        0xcb -> set 1 (Reg8 E)
        0xcc -> set 1 (Reg8 H)
        0xcd -> set 1 (Reg8 L)
        0xce -> set 1 P_HL
        0xd7 -> set 2 (Reg8 A)
        0xd0 -> set 2 (Reg8 B)
        0xd1 -> set 2 (Reg8 C)
        0xd2 -> set 2 (Reg8 D)
        0xd3 -> set 2 (Reg8 E)
        0xd4 -> set 2 (Reg8 H)
        0xd5 -> set 2 (Reg8 L)
        0xd6 -> set 2 P_HL
        0xdf -> set 3 (Reg8 A)
        0xd8 -> set 3 (Reg8 B)
        0xd9 -> set 3 (Reg8 C)
        0xda -> set 3 (Reg8 D)
        0xdb -> set 3 (Reg8 E)
        0xdc -> set 3 (Reg8 H)
        0xdd -> set 3 (Reg8 L)
        0xde -> set 3 P_HL
        0xe7 -> set 4 (Reg8 A)
        0xe0 -> set 4 (Reg8 B)
        0xe1 -> set 4 (Reg8 C)
        0xe2 -> set 4 (Reg8 D)
        0xe3 -> set 4 (Reg8 E)
        0xe4 -> set 4 (Reg8 H)
        0xe5 -> set 4 (Reg8 L)
        0xe6 -> set 4 P_HL
        0xef -> set 5 (Reg8 A)
        0xe8 -> set 5 (Reg8 B)
        0xe9 -> set 5 (Reg8 C)
        0xea -> set 5 (Reg8 D)
        0xeb -> set 5 (Reg8 E)
        0xec -> set 5 (Reg8 H)
        0xed -> set 5 (Reg8 L)
        0xee -> set 5 P_HL
        0xf7 -> set 6 (Reg8 A)
        0xf0 -> set 6 (Reg8 B)
        0xf1 -> set 6 (Reg8 C)
        0xf2 -> set 6 (Reg8 D)
        0xf3 -> set 6 (Reg8 E)
        0xf4 -> set 6 (Reg8 H)
        0xf5 -> set 6 (Reg8 L)
        0xf6 -> set 6 P_HL
        0xff -> set 7 (Reg8 A)
        0xf8 -> set 7 (Reg8 B)
        0xf9 -> set 7 (Reg8 C)
        0xfa -> set 7 (Reg8 D)
        0xfb -> set 7 (Reg8 E)
        0xfc -> set 7 (Reg8 H)
        0xfd -> set 7 (Reg8 L)
        0xfe -> set 7 P_HL

        0x87 -> res 0 (Reg8 A)
        0x80 -> res 0 (Reg8 B)
        0x81 -> res 0 (Reg8 C)
        0x82 -> res 0 (Reg8 D)
        0x83 -> res 0 (Reg8 E)
        0x84 -> res 0 (Reg8 H)
        0x85 -> res 0 (Reg8 L)
        0x86 -> res 0 P_HL
        0x8f -> res 1 (Reg8 A)
        0x88 -> res 1 (Reg8 B)
        0x89 -> res 1 (Reg8 C)
        0x8a -> res 1 (Reg8 D)
        0x8b -> res 1 (Reg8 E)
        0x8c -> res 1 (Reg8 H)
        0x8d -> res 1 (Reg8 L)
        0x8e -> res 1 P_HL
        0x97 -> res 2 (Reg8 A)
        0x90 -> res 2 (Reg8 B)
        0x91 -> res 2 (Reg8 C)
        0x92 -> res 2 (Reg8 D)
        0x93 -> res 2 (Reg8 E)
        0x94 -> res 2 (Reg8 H)
        0x95 -> res 2 (Reg8 L)
        0x96 -> res 2 P_HL
        0x9f -> res 3 (Reg8 A)
        0x98 -> res 3 (Reg8 B)
        0x99 -> res 3 (Reg8 C)
        0x9a -> res 3 (Reg8 D)
        0x9b -> res 3 (Reg8 E)
        0x9c -> res 3 (Reg8 H)
        0x9d -> res 3 (Reg8 L)
        0x9e -> res 3 P_HL
        0xa7 -> res 4 (Reg8 A)
        0xa0 -> res 4 (Reg8 B)
        0xa1 -> res 4 (Reg8 C)
        0xa2 -> res 4 (Reg8 D)
        0xa3 -> res 4 (Reg8 E)
        0xa4 -> res 4 (Reg8 H)
        0xa5 -> res 4 (Reg8 L)
        0xa6 -> res 4 P_HL
        0xaf -> res 5 (Reg8 A)
        0xa8 -> res 5 (Reg8 B)
        0xa9 -> res 5 (Reg8 C)
        0xaa -> res 5 (Reg8 D)
        0xab -> res 5 (Reg8 E)
        0xac -> res 5 (Reg8 H)
        0xad -> res 5 (Reg8 L)
        0xae -> res 5 P_HL
        0xb7 -> res 6 (Reg8 A)
        0xb0 -> res 6 (Reg8 B)
        0xb1 -> res 6 (Reg8 C)
        0xb2 -> res 6 (Reg8 D)
        0xb3 -> res 6 (Reg8 E)
        0xb4 -> res 6 (Reg8 H)
        0xb5 -> res 6 (Reg8 L)
        0xb6 -> res 6 P_HL
        0xbf -> res 7 (Reg8 A)
        0xb8 -> res 7 (Reg8 B)
        0xb9 -> res 7 (Reg8 C)
        0xba -> res 7 (Reg8 D)
        0xbb -> res 7 (Reg8 E)
        0xbc -> res 7 (Reg8 H)
        0xbd -> res 7 (Reg8 L)
        0xbe -> res 7 P_HL
        _ -> error $  "CPU.execute: undefined instruction 0xcb " ++ showHex codeCB

    _ -> error $ "CPU.execute: undefined instruction " ++ showHex code

serial :: GB ()
serial = do
  CPU {..} <- getCPU
  sc <- liftIO $ readGBReg mbc SC
  when (testBit sc 7) $ do
    let 
      ls = [512, 256, 16, 8] :: [Word64]
      clock = ls !! fi (sc .&. 0b11)
    sys <- readReg64 SysCounter
    when (sys `mod` clock == 0) $ do
      sb <- liftIO $ readGBReg mbc SB
      liftIO $ writeLogger serialLogger sb
      liftIO $ writeGBReg mbc SC $ clearBit sc 7
      liftIO $ modifyGBReg mbc IF $ flip setBit 3

timer :: GB ()
timer = do
  CPU {..} <- getCPU
  sys <- readReg64 SysCounter
  when (sys `mod` 256 == 0) $ do
    liftIO $ modifyGBReg mbc DIV (+ 1)

  tac <- liftIO $ readGBReg mbc TAC
  when (testBit tac 2) $ do
    let 
      ls = [1024, 16, 64, 256] :: [Word64]
      clock = ls !! fi (tac .&. 0b11)
    when (sys `mod` clock == 0) $ do
      tima <- liftIO $ readGBReg mbc TIMA
      let (tima', carry, _) = addCarryHalf tima (1 :: Word8)
      if carry then do
        tma <- liftIO $ readGBReg mbc TMA
        liftIO $ modifyGBReg mbc IF $ flip setBit 2
        liftIO $ writeGBReg mbc TIMA tma
      else do
        liftIO $ writeGBReg mbc TIMA tima'

joypad :: GB ()
joypad = do
  CPU {..} <- getCPU
  jb <- liftIO $ readStore joypadBuffer 0
  joyp <- liftIO $ readGBReg mbc JOYP
  when (not $ testBit joyp 4) $ do
    liftIO $ writeGBReg mbc JOYP (0b100000 .|. jb .&. 0b1111)
    liftIO $ modifyGBReg mbc IF $ flip setBit 4
  when (not $ testBit joyp 5) $ do
    liftIO $ writeGBReg mbc JOYP (0b010000 .|. jb `shiftR` 4)
    liftIO $ modifyGBReg mbc IF $ flip setBit 4
    
interrupt :: GB ()
interrupt = do
  CPU {..} <- getCPU
  enable <- liftIO $ readGBReg mbc IE
  request <- liftIO $ readGBReg mbc IF
  when (enable .&. request /= 0) $ do
    writeReg8 Halt 0

  ime <- readReg8 IME
  when (ime == 1) $ do
    writeReg8 Halt 0
    let
      (addr, n, _ :: String) = 
        if testBit enable 0 && testBit request 0 then (0x40, 0, "VBlank")
        else if testBit enable 1 && testBit request 1 then (0x48, 1, "LSTAT")
        else if testBit enable 2 && testBit request 2 then (0x50, 2, "Timer")
        else if testBit enable 3 && testBit request 3 then (0x58, 3, "Serial")
        else if testBit enable 4 && testBit request 4 then (0x60, 4, "Joypad")
        else (0, 0, "")
    when (addr /= 0) $ do
      pc <- readReg16 PC
      push16 pc
      writeReg16 PC addr
      writeReg8 IME 0
      writeReg8 Halt 0
      liftIO $ modifyGBReg mbc IF $ flip clearBit n
      tick
      tick
      tick

stepCPU :: GB ()
stepCPU = do
  writeReg8 Cycle 0

  halting <- readReg8 Halt
  if halting == 1 then
    tick
  else do
    executeInstruction
    modifyReg64 ExeCounter (+ 1)

  cycle <- readReg8 Cycle
  replicateM_ (fi cycle * 4) $ do
    serial
    timer
    joypad
    interrupt
    modifyReg64 SysCounter (+ 1)
