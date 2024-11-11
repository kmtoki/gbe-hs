module GB.CPU (
  CPU(..),
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

data CPU = CPU { 
  mbc :: MBC,
  cpuLogger :: Logger CPULog,
  serialLogger :: Logger Word8,
  joypadBuffer :: Store Word8,

  regs8 :: Store Word8,
  regs16 :: Store Word16,
  regs64 :: Store Word64
  }

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

log :: CPU -> String -> CPUOpLog -> CPUOpLog -> CPUOpLog -> IO ()
log cpu@(CPU {..}) instruction op1 op2 op3 = do
  isLogging <- readReg8 cpu IsLogging
  when (isLogging == 1) $ do
    [a,f,b,c,d,e,h,l,ime,halting] <- mapM (readReg8 cpu) [A,F,B,C,D,E,H,L,IME,Halt]
    [sp,pc'] <- mapM (readReg16 cpu) [SP,PC]
    let pc = pc' - 1
    codes <- mapM (readMBC mbc . (+ pc)) [0..3]
    stack <- mapM (readMBC mbc . (sp -)) [0..3]
    [exeCounter] <- mapM (readReg64 cpu) [ExeCounter]
    [regIF,regIE] <- mapM (readGBReg mbc) [IF,IE]
    mbcROMBank <- readMBCROMBank mbc
    writeLogger cpuLogger $ CPULog {..}

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


readReg8 :: CPU -> CPURegisters8 -> IO Word8
readReg8 (CPU {..}) r = readStore regs8 $ fromEnum r

readReg16 :: CPU -> CPURegisters16 -> IO Word16
readReg16 (CPU {..}) r = readStore regs16 $ fromEnum r

readReg64 :: CPU -> CPURegisters64 -> IO Word64
readReg64 (CPU {..}) r = readStore regs64 $ fromEnum r

writeReg8 :: CPU -> CPURegisters8 -> Word8 -> IO ()
writeReg8 (CPU {..}) r n = writeStore regs8 (fromEnum r) n

writeReg16 :: CPU -> CPURegisters16 -> Word16 -> IO ()
writeReg16 (CPU {..}) r n = writeStore regs16 (fromEnum r) n

writeReg64 :: CPU -> CPURegisters64 -> Word64 -> IO ()
writeReg64 (CPU {..}) r n = writeStore regs64 (fromEnum r) n

modifyReg8 :: CPU -> CPURegisters8 -> (Word8 -> Word8) -> IO ()
modifyReg8 cpu r f = readReg8 cpu r >>= writeReg8 cpu r . f

modifyReg16 :: CPU -> CPURegisters16 -> (Word16 -> Word16) -> IO ()
modifyReg16 cpu r f = readReg16 cpu r >>= writeReg16 cpu r . f

modifyReg64 :: CPU -> CPURegisters64 -> (Word64 -> Word64) -> IO ()
modifyReg64 cpu r f = readReg64 cpu r >>= writeReg64 cpu r . f


readFlag :: CPU -> CPUFlags -> IO Bool
readFlag cpu flag = do
  f <- readReg8 cpu F
  pure $ testBit f $ 4 + fromEnum flag

writeFlag :: CPU -> CPUFlags -> Bool -> IO ()
writeFlag cpu flag bool = do
  f <- readReg8 cpu F
  let f' = (if bool then setBit else clearBit) f (4 + fromEnum flag)
  writeReg8 cpu F $ f' .&. 0b11110000


setIsLogging :: CPU -> Bool -> IO ()
setIsLogging cpu = writeReg8 cpu IsLogging . fi . fromEnum


readSerial :: CPU -> IO [Word8]
readSerial (CPU {..}) = readAllLogger serialLogger


newCPU :: MBC -> IO CPU
newCPU mbc = do
  cpuLogger <- newLogger 0xffff (CPULog 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [0,0,0,0] [0,0,0,0] "" None None None)
  serialLogger <- newLogger 0xffff 0
  joypadBuffer <- newStore 1 0xff
  r8 <- newStore 0xff 0
  r16 <- newStore 0xff 0
  r64 <- newStore 0xff 0

  let cpu = CPU mbc cpuLogger serialLogger joypadBuffer r8 r16 r64

  writeReg16 cpu SP 0xfffe
  writeReg16 cpu PC 0x100
  pure $ cpu

tick :: CPU -> IO ()
tick cpu = do
  c <- readReg8 cpu Cycle
  writeReg8 cpu Cycle $ c + 1

fetch8 :: CPU -> IO Word8
fetch8 cpu@(CPU {..}) = do
  pc <- readReg16 cpu PC
  writeReg16 cpu PC $ pc + 1
  tick cpu
  readMBC mbc pc

fetch16 :: CPU -> IO Word16
fetch16 cpu = do
  l <- fetch8 cpu
  h <- fetch8 cpu
  pure $ toWord16 h l

push8 :: CPU -> Word8 -> IO ()
push8 cpu@(CPU {..}) n = do
  sp <- subtract 1 <$> readReg16 cpu SP
  writeReg16 cpu SP sp
  writeMBC mbc sp n
  tick cpu

push16 :: CPU -> Word16 -> IO ()
push16 cpu n = do
  let (h, l) = sepWord16 n
  push8 cpu h
  push8 cpu l

pop8 :: CPU -> IO Word8
pop8 cpu@(CPU {..}) = do
  sp <- readReg16 cpu SP
  n <- readMBC mbc sp
  modifyReg16 cpu SP (+ 1)
  tick cpu
  pure n

pop16 :: CPU -> IO Word16
pop16 cpu = do
  l <- pop8 cpu
  h <- pop8 cpu
  pure $ toWord16 h l

readOp8 :: CPU -> Op8 -> IO Word8
readOp8 cpu@(CPU {..}) op = case op of
  Reg8 r -> readReg8 cpu r
  A_ -> readReg8 cpu A
  N -> fetch8 cpu
  P_BC -> readOp16 cpu BC >>= readMBC mbc
  P_DE -> readOp16 cpu DE >>= readMBC mbc
  P_HL -> readOp16 cpu HL >>= readMBC mbc
  P_NN_8 -> fetch16 cpu >>= readMBC mbc
  P_FF00_N -> fetch8 cpu >>= readMBC mbc . (0xff00 +) . fi
  P_FF00_C -> readReg8 cpu C >>= readMBC mbc . (0xff00 +) . fi
  P_HL_INC -> do
    hl <- readOp16 cpu HL
    writeOp16 cpu HL $ hl + 1
    readMBC mbc hl
  P_HL_DEC -> do
    hl <- readOp16 cpu HL
    writeOp16 cpu HL $ hl - 1
    readMBC mbc hl

readOp16 :: CPU -> Op16 -> IO Word16
readOp16 cpu op = case op of
  Reg16 r -> readReg16 cpu r
  AF -> toWord16 <$> readReg8 cpu A <*> readReg8 cpu F
  BC -> toWord16 <$> readReg8 cpu B <*> readReg8 cpu C
  DE -> toWord16 <$> readReg8 cpu D <*> readReg8 cpu E
  HL -> toWord16 <$> readReg8 cpu H <*> readReg8 cpu L
  NN -> fetch16 cpu
  _ -> error $ "CPU.readOp16 unexpect " ++ show op

writeOp8 :: CPU -> Op8 -> Word8 -> IO ()
writeOp8 cpu@(CPU {..}) op n = case op of
  Reg8 r -> writeReg8 cpu r n
  A_ -> writeReg8 cpu A n
  P_BC -> readOp16 cpu BC >>= \a -> writeMBC mbc a n
  P_DE -> readOp16 cpu DE >>= \a -> writeMBC mbc a n
  P_HL -> readOp16 cpu HL >>= \a -> writeMBC mbc a n
  P_NN_8 -> fetch16 cpu >>= \a -> writeMBC mbc a n
  P_FF00_C -> readReg8 cpu C >>= \c -> writeMBC mbc (0xff00 + fi c) n
  P_FF00_N -> fetch8 cpu >>= \a -> writeMBC mbc (0xff00 + fi a) n
  P_HL_INC -> do
    hl <- readOp16 cpu HL
    writeOp16 cpu HL $ hl + 1
    writeMBC mbc hl n
  P_HL_DEC -> do
    hl <- readOp16 cpu HL
    writeOp16 cpu HL $ hl - 1
    writeMBC mbc hl n
  _ -> error $ "CPU.writeOp8 unexpect " ++ show op

writeOp16 :: CPU -> Op16 -> Word16 -> IO ()
writeOp16 cpu@(CPU {..}) op n = 
  let (h,l) = sepWord16 n in 
    case op of
      Reg16 r -> writeReg16 cpu r n
      AF -> writeReg8 cpu A h >> writeReg8 cpu F (l .&. 0b11110000)
      BC -> writeReg8 cpu B h >> writeReg8 cpu C l
      DE -> writeReg8 cpu D h >> writeReg8 cpu E l
      HL -> writeReg8 cpu H h >> writeReg8 cpu L l
      P_NN_16 -> fetch16 cpu >>= \a -> writeMBC mbc a l >> writeMBC mbc (a + 1) h
      _ -> error $ "CPU.writeOp16 unexpect " ++ show op

condFlag :: CPU -> OpCond -> IO Bool
condFlag cpu op = case op of
  IsZero -> readFlag cpu Zero
  NotZero -> not <$> readFlag cpu Zero
  IsCarry -> readFlag cpu Carry
  NotCarry -> not <$> readFlag cpu Carry
  Always -> pure True


ld8 :: CPU -> Op8 -> Op8 -> IO ()
ld8 cpu op1 op2 = do
  log cpu "LD" (Log8 op1) (Log8 op2) None
  n <- readOp8 cpu op2
  writeOp8 cpu op1 n

ld16 :: CPU -> Op16 -> Op16 -> IO ()
ld16 cpu op1 op2 = do
  log cpu "LD" (Log16 op1) (Log16 op2) None
  n <- readOp16 cpu op2
  writeOp16 cpu op1 n

ld16_hl_sp_n :: CPU -> IO ()
ld16_hl_sp_n cpu = do
  n <- fetch8 cpu

  modifyReg16 cpu PC (subtract 1)
  log cpu "LD" (Log16 HL) (Log16 $ Reg16 SP) (LogInfoI8 $ fi n)
  modifyReg16 cpu PC (+ 1)

  sp <- readReg16 cpu SP
  let (a, carry, half) = addCarryHalf sp (fi n :: Int8)

  writeOp16 cpu HL a
  writeFlag cpu Carry carry
  writeFlag cpu Half half
  writeFlag cpu Negative False
  writeFlag cpu Zero False
  tick cpu

push :: CPU -> Op16 -> IO ()
push cpu op = do
  log cpu "PUSH" (Log16 op) None None
  n <- readOp16 cpu op
  tick cpu
  push16 cpu n
  
pop :: CPU -> Op16 -> IO ()
pop cpu op = do
  log cpu "POP" (Log16 op) None None
  n <- pop16 cpu
  writeOp16 cpu op n

add :: CPU -> Op8 -> IO ()
add cpu op = do
  log cpu "ADD" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op
  let (a', carry, half) = addCarryHalf a n
  writeReg8 cpu A a'
  writeFlag cpu Carry carry
  writeFlag cpu Half half
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a' == 0

adc :: CPU -> Op8 -> IO ()
adc cpu op = do
  log cpu "ADC" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op
  c <- readFlag cpu Carry
  let (a', carry, half) = addCarryHalf a n
  let (a'', carry', half') = addCarryHalf a' (fi $ fromEnum c :: Word8)
  writeReg8 cpu A a''
  writeFlag cpu Carry $ carry || carry'
  writeFlag cpu Half $ half || half'
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a'' == 0

sub :: CPU -> Op8 -> IO ()
sub cpu op = do
  log cpu "SUB" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op
  let (a', carry, half) = subCarryHalf a n
  writeReg8 cpu A a'
  writeFlag cpu Carry carry
  writeFlag cpu Half half
  writeFlag cpu Negative True
  writeFlag cpu Zero $ a' == 0

sbc :: CPU -> Op8 -> IO ()
sbc cpu op = do
  log cpu "SBC" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op
  c <- readFlag cpu Carry
  let (a', carry, half) = subCarryHalf a n
  let (a'', carry', half') = subCarryHalf a' (fi $ fromEnum c :: Word8)
  writeReg8 cpu A a''
  writeFlag cpu Carry $ carry || carry'
  writeFlag cpu Half $ half || half'
  writeFlag cpu Negative True
  writeFlag cpu Zero $ a'' == 0

and :: CPU -> Op8 -> IO ()
and cpu op = do
  log cpu "AND" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op 
  let a' = a .&. n
  writeReg8 cpu A a'
  writeFlag cpu Carry False
  writeFlag cpu Half True
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a' == 0

or :: CPU -> Op8 -> IO ()
or cpu op = do
  log cpu "OR" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op 
  let a' = a .|. n
  writeReg8 cpu A a'
  writeFlag cpu Carry False
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a' == 0

xor :: CPU -> Op8 -> IO ()
xor cpu op = do
  log cpu "XOR" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op 
  let a' = a .^. n
  writeReg8 cpu A a'
  writeFlag cpu Carry False
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a' == 0

cp :: CPU -> Op8 -> IO ()
cp cpu op = do
  log cpu "CP" (Log8 op) None None
  a <- readReg8 cpu A
  n <- readOp8 cpu op 
  let (a',carry,half) = subCarryHalf a n
  writeFlag cpu Carry carry
  writeFlag cpu Half half
  writeFlag cpu Negative True
  writeFlag cpu Zero $ a' == 0

inc8 :: CPU -> Op8 -> IO ()
inc8 cpu op = do
  log cpu "INC" (Log8 op) None None
  a <- readOp8 cpu op
  let (a',_,half) = addCarryHalf a (1 :: Word8)
  writeOp8 cpu op a'
  writeFlag cpu Half half
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a' == 0

dec8 :: CPU -> Op8 -> IO ()
dec8 cpu op = do
  log cpu "DEC" (Log8 op) None None
  a <- readOp8 cpu op
  let (a',_,half) = subCarryHalf a (1 :: Word8)
  writeOp8 cpu op a'
  writeFlag cpu Half half
  writeFlag cpu Negative True
  writeFlag cpu Zero $ a' == 0

add_hl :: CPU -> Op16 -> IO ()
add_hl cpu op = do
  log cpu "DEC" (Log16 HL) (Log16 op) None
  hl <- readOp16 cpu HL
  n <- readOp16 cpu op
  let (hl',carry,half) = addCarryHalf hl n
  writeOp16 cpu HL hl'
  writeFlag cpu Carry carry
  writeFlag cpu Half half
  writeFlag cpu Negative False

add_sp_n :: CPU -> IO ()
add_sp_n cpu = do
  n <- fetch8 cpu
  sp <- readReg16 cpu SP

  modifyReg16 cpu PC (subtract 1)
  log cpu "ADD" (Log16 $ Reg16 SP) (Log8 N) (LogInfoI8 $ fi n)
  modifyReg16 cpu PC (+ 1)

  let (sp',carry,half) = addCarryHalf sp (fi n :: Int8)
  writeReg16 cpu SP sp'
  writeFlag cpu Carry carry
  writeFlag cpu Half half
  writeFlag cpu Negative False
  writeFlag cpu Zero False
  tick cpu
  tick cpu

inc16 :: CPU -> Op16 -> IO ()
inc16 cpu op = do
  log cpu "INC" (Log16 op) None None
  a <- readOp16 cpu op
  let (a',_,_) = addCarryHalf a (1 :: Word16)
  writeOp16 cpu op a'

dec16 :: CPU -> Op16 -> IO ()
dec16 cpu op = do
  log cpu "DEC" (Log16 op) None None
  a <- readOp16 cpu op
  let (a',_,_) = subCarryHalf a (1 :: Word16)
  writeOp16 cpu op a'

daa :: CPU -> IO ()
daa cpu = do
  log cpu "DAA" None None None
  a <- readReg8 cpu A
  carry <- readFlag cpu Carry
  half <- readFlag cpu Half
  negative <- readFlag cpu Negative

  let 
    adjust = (if carry then 0x60 else 0) .|. (if half then 0x06 else 0)
    adjust' = 
      if not negative then
        adjust .|. (if a .&. 0x0f > 0x09 then 0x06 else 0) .|. (if a > 0x99 then 0x60 else 0)
      else
        adjust
    a' = if not negative then a + adjust' else a - adjust'

  writeReg8 cpu A a'
  writeFlag cpu Carry $ adjust' >= 0x60
  writeFlag cpu Half False 
  writeFlag cpu Zero $ a' == 0

cpl :: CPU -> IO ()
cpl cpu = do
  log cpu "CPL" None None None
  modifyReg8 cpu A (.^. 0xff)
  writeFlag cpu Half True
  writeFlag cpu Negative True

ccf :: CPU -> IO ()
ccf cpu = do
  log cpu "CCF" None None None
  carry <- readFlag cpu Carry
  writeFlag cpu Carry $ not carry
  writeFlag cpu Half False
  writeFlag cpu Negative False

scf :: CPU -> IO ()
scf cpu = do
  log cpu "SCF" None None None
  writeFlag cpu Carry True
  writeFlag cpu Half False
  writeFlag cpu Negative False

di :: CPU -> IO ()
di cpu = do
  log cpu "DI" None None None
  writeReg8 cpu IME 0

ei :: CPU -> IO ()
ei cpu = do
  log cpu "EI" None None None
  writeReg8 cpu IME 1

halt :: CPU -> IO ()
halt cpu = do
  log cpu "HALT" None None None
  writeReg8 cpu Halt 1

stop :: CPU -> IO ()
stop cpu = do
  log cpu "STOP" None None None
  --writeReg8 cpu Halt 1

nop :: CPU -> IO ()
nop cpu = do
  log cpu "NOP" None None None
  tick cpu

jp :: CPU -> OpCond -> IO ()
jp cpu op = do
  nn <- fetch16 cpu

  modifyReg16 cpu PC (subtract 2)
  log cpu "JP" (LogCond op) None $ LogInfoW16 nn
  modifyReg16 cpu PC (+ 2)

  bool <- condFlag cpu op
  when bool $ do
    writeReg16 cpu PC nn
    tick cpu

jp_p_hl :: CPU -> IO ()
jp_p_hl cpu = do
  hl <- readOp16 cpu HL 
  log cpu "JP" (Log16 HL) None (LogInfoW16 hl)
  writeReg16 cpu PC hl
  tick cpu


jr :: CPU -> OpCond -> IO ()
jr cpu op = do
  n <- fetch8 cpu

  modifyReg16 cpu PC (subtract 1) 
  log cpu "JR" (LogCond op) None (LogInfoI8 $ fi n)
  modifyReg16 cpu PC (+ 1)

  bool <- condFlag cpu op
  when bool $ do
    pc <- readReg16 cpu PC
    let (pc',_,_) = addCarryHalf pc (fi n :: Int8)
    writeReg16 cpu PC pc'
    tick cpu

call :: CPU -> OpCond -> IO ()
call cpu op = do
  nn <- fetch16 cpu

  modifyReg16 cpu PC (subtract 2)
  log cpu "CALL" (LogCond op) None $ LogInfoW16 nn
  modifyReg16 cpu PC (+ 2)

  bool <- condFlag cpu op
  when bool $ do
    tick cpu
    pc <- readReg16 cpu PC
    push16 cpu pc
    writeReg16 cpu PC nn

ret :: CPU -> OpCond -> IO ()
ret cpu op = do
  modifyReg16 cpu SP (+ 1)
  log cpu "RET" (LogCond op) None None
  modifyReg16 cpu SP (subtract 1)

  bool <- condFlag cpu op
  when bool $ do
    pc <- pop16 cpu
    writeReg16 cpu PC pc
    tick cpu

reti :: CPU -> IO ()
reti cpu = do
  pc <- pop16 cpu

  modifyReg16 cpu SP (subtract 2)
  log cpu "RETI" None None $ LogInfoW16 pc
  modifyReg16 cpu SP (+ 2)

  writeReg16 cpu PC pc
  tick cpu
  writeReg8 cpu IME 1

rst :: CPU -> Word16 -> IO ()
rst cpu nn = do
  log cpu "RST" None None $ LogInfoW16 nn
  tick cpu
  pc <- readReg16 cpu PC
  push16 cpu pc
  writeReg16 cpu PC nn

swap :: CPU -> Op8 -> IO ()
swap cpu op = do
  log cpu "SWAP" (Log8 op) None None
  r <- readOp8 cpu op
  let a = (r `shiftL` 4) .|. (r `shiftR` 4)
  writeOp8 cpu op a
  writeFlag cpu Carry False
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a == 0

rlc :: CPU -> Op8 -> IO ()
rlc cpu op = do
  log cpu "RLC" (Log8 op) None None
  r <- readOp8 cpu op
  let 
    c = r `shiftR` 7
    a = (r `shiftL` 1) .|. c
  writeOp8 cpu op a
  writeFlag cpu Carry $ c == 1
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ if op == A_ then False else a == 0

rl :: CPU -> Op8 -> IO ()
rl cpu op = do
  log cpu "RL" (Log8 op) None None
  r <- readOp8 cpu op
  carry <- readFlag cpu Carry
  let a = (r `shiftL` 1) .|. (toNum carry)
  writeOp8 cpu op a
  writeFlag cpu Carry $ r `shiftR` 7 == 1
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ if op == A_ then False else a == 0

rrc :: CPU -> Op8 -> IO ()
rrc cpu op = do
  log cpu "RRC" (Log8 op) None None
  r <- readOp8 cpu op
  let 
    c = r .&. 1
    a = (r `shiftL` 7) .|. (r `shiftR` 1)
  writeOp8 cpu op a
  writeFlag cpu Carry $ c == 1
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ if op == A_ then False else a == 0

rr :: CPU -> Op8 -> IO ()
rr cpu op = do
  log cpu "RR" (Log8 op) None None
  r <- readOp8 cpu op
  carry <- readFlag cpu Carry
  let a = ((toNum carry :: Word8) `shiftL` 7) .|. (r `shiftR` 1)
  writeOp8 cpu op a
  writeFlag cpu Carry $ r .&. 1 == 1
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ if op == A_ then False else a == 0

sla :: CPU -> Op8 -> IO ()
sla cpu op = do
  log cpu "SLA" (Log8 op) None None
  r <- readOp8 cpu op
  let a = r `shiftL` 1
  writeOp8 cpu op a
  writeFlag cpu Carry $ r `shiftR` 7 == 1
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a == 0

sra :: CPU -> Op8 -> IO ()
sra cpu op = do
  log cpu "SRA" (Log8 op) None None
  r <- readOp8 cpu op
  let a = (r .&. 0b10000000) .|. (r `shiftR` 1)
  writeOp8 cpu op a
  writeFlag cpu Carry $ r .&. 1 == 1
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a == 0

srl :: CPU -> Op8 -> IO ()
srl cpu op = do
  log cpu "SRL" (Log8 op) None None
  r <- readOp8 cpu op
  let a = r `shiftR` 1
  writeOp8 cpu op a
  writeFlag cpu Carry $ r .&. 1 == 1
  writeFlag cpu Half False
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a == 0

bit :: CPU -> Word8 -> Op8 -> IO ()
bit cpu n op = do
  log cpu "BIT" (Log8 op) (LogInfoW8 n) None
  r <- readOp8 cpu op
  let a = testBit r $ fi n
  writeFlag cpu Half True
  writeFlag cpu Negative False
  writeFlag cpu Zero $ a == False 

set :: CPU -> Word8 -> Op8 -> IO ()
set cpu n op = do
  log cpu "SET" (Log8 op) (LogInfoW8 n) None
  r <- readOp8 cpu op
  let a = setBit r $ fi n
  writeOp8 cpu op a

res :: CPU -> Word8 -> Op8 -> IO ()
res cpu n op = do
  log cpu "RET" (Log8 op) (LogInfoW8 n) None
  r <- readOp8 cpu op
  let a = clearBit r $ fi n
  writeOp8 cpu op a


executeInstruction :: CPU -> IO ()
executeInstruction cpu = do
  code <- fetch8 cpu
  case code of
    0x3e -> ld8 cpu (Reg8 A) N
    0x06 -> ld8 cpu (Reg8 B) N
    0x0e -> ld8 cpu (Reg8 C) N
    0x16 -> ld8 cpu (Reg8 D) N
    0x1e -> ld8 cpu (Reg8 E) N
    0x26 -> ld8 cpu (Reg8 H) N
    0x2e -> ld8 cpu (Reg8 L) N
    0x7f -> ld8 cpu (Reg8 A) (Reg8 A)
    0x78 -> ld8 cpu (Reg8 A) (Reg8 B)
    0x79 -> ld8 cpu (Reg8 A) (Reg8 C)
    0x7a -> ld8 cpu (Reg8 A) (Reg8 D)
    0x7b -> ld8 cpu (Reg8 A) (Reg8 E)
    0x7c -> ld8 cpu (Reg8 A) (Reg8 H)
    0x7d -> ld8 cpu (Reg8 A) (Reg8 L)
    0x7e -> ld8 cpu (Reg8 A) P_HL
    0x0a -> ld8 cpu (Reg8 A) P_BC
    0x1a -> ld8 cpu (Reg8 A) P_DE
    0x47 -> ld8 cpu (Reg8 B) (Reg8 A)
    0x40 -> ld8 cpu (Reg8 B) (Reg8 B)
    0x41 -> ld8 cpu (Reg8 B) (Reg8 C)
    0x42 -> ld8 cpu (Reg8 B) (Reg8 D)
    0x43 -> ld8 cpu (Reg8 B) (Reg8 E)
    0x44 -> ld8 cpu (Reg8 B) (Reg8 H)
    0x45 -> ld8 cpu (Reg8 B) (Reg8 L)
    0x46 -> ld8 cpu (Reg8 B) P_HL
    0x4f -> ld8 cpu (Reg8 C) (Reg8 A)
    0x48 -> ld8 cpu (Reg8 C) (Reg8 B)
    0x49 -> ld8 cpu (Reg8 C) (Reg8 C)
    0x4a -> ld8 cpu (Reg8 C) (Reg8 D)
    0x4b -> ld8 cpu (Reg8 C) (Reg8 E)
    0x4c -> ld8 cpu (Reg8 C) (Reg8 H)
    0x4d -> ld8 cpu (Reg8 C) (Reg8 L)
    0x4e -> ld8 cpu (Reg8 C) P_HL
    0x57 -> ld8 cpu (Reg8 D) (Reg8 A)
    0x50 -> ld8 cpu (Reg8 D) (Reg8 B)
    0x51 -> ld8 cpu (Reg8 D) (Reg8 C)
    0x52 -> ld8 cpu (Reg8 D) (Reg8 D)
    0x53 -> ld8 cpu (Reg8 D) (Reg8 E)
    0x54 -> ld8 cpu (Reg8 D) (Reg8 H)
    0x55 -> ld8 cpu (Reg8 D) (Reg8 L)
    0x56 -> ld8 cpu (Reg8 D) P_HL
    0x5f -> ld8 cpu (Reg8 E) (Reg8 A)
    0x58 -> ld8 cpu (Reg8 E) (Reg8 B)
    0x59 -> ld8 cpu (Reg8 E) (Reg8 C)
    0x5a -> ld8 cpu (Reg8 E) (Reg8 D)
    0x5b -> ld8 cpu (Reg8 E) (Reg8 E)
    0x5c -> ld8 cpu (Reg8 E) (Reg8 H)
    0x5d -> ld8 cpu (Reg8 E) (Reg8 L)
    0x5e -> ld8 cpu (Reg8 E) P_HL
    0x67 -> ld8 cpu (Reg8 H) (Reg8 A)
    0x60 -> ld8 cpu (Reg8 H) (Reg8 B)
    0x61 -> ld8 cpu (Reg8 H) (Reg8 C)
    0x62 -> ld8 cpu (Reg8 H) (Reg8 D)
    0x63 -> ld8 cpu (Reg8 H) (Reg8 E)
    0x64 -> ld8 cpu (Reg8 H) (Reg8 H)
    0x65 -> ld8 cpu (Reg8 H) (Reg8 L)
    0x66 -> ld8 cpu (Reg8 H) P_HL
    0x6f -> ld8 cpu (Reg8 L) (Reg8 A)
    0x68 -> ld8 cpu (Reg8 L) (Reg8 B)
    0x69 -> ld8 cpu (Reg8 L) (Reg8 C)
    0x6a -> ld8 cpu (Reg8 L) (Reg8 D)
    0x6b -> ld8 cpu (Reg8 L) (Reg8 E)
    0x6c -> ld8 cpu (Reg8 L) (Reg8 H)
    0x6d -> ld8 cpu (Reg8 L) (Reg8 L)
    0x6e -> ld8 cpu (Reg8 L) P_HL

    0x70 -> ld8 cpu P_HL (Reg8 B)
    0x71 -> ld8 cpu P_HL (Reg8 C)
    0x72 -> ld8 cpu P_HL (Reg8 D)
    0x73 -> ld8 cpu P_HL (Reg8 E)
    0x74 -> ld8 cpu P_HL (Reg8 H)
    0x75 -> ld8 cpu P_HL (Reg8 L)
    0x36 -> ld8 cpu P_HL N
    0x02 -> ld8 cpu P_BC (Reg8 A)
    0x12 -> ld8 cpu P_DE (Reg8 A)
    0x77 -> ld8 cpu P_HL (Reg8 A)
    0xea -> ld8 cpu P_NN_8 (Reg8 A)

    0xf0 -> ld8 cpu (Reg8 A) P_FF00_N
    0xf2 -> ld8 cpu (Reg8 A) P_FF00_C
    0xfa -> ld8 cpu (Reg8 A) P_NN_8
    0xe0 -> ld8 cpu P_FF00_N (Reg8 A)
    0xe2 -> ld8 cpu P_FF00_C (Reg8 A)

    0x22 -> ld8 cpu P_HL_INC (Reg8 A)
    0x2a -> ld8 cpu (Reg8 A) P_HL_INC
    0x32 -> ld8 cpu P_HL_DEC (Reg8 A)
    0x3a -> ld8 cpu (Reg8 A) P_HL_DEC

    0x01 -> ld16 cpu BC NN
    0x11 -> ld16 cpu DE NN
    0x21 -> ld16 cpu HL NN
    0x31 -> ld16 cpu (Reg16 SP) NN
    0xf9 -> ld16 cpu (Reg16 SP) HL
    0x08 -> ld16 cpu P_NN_16 (Reg16 SP)
    0xf8 -> ld16_hl_sp_n cpu 

    0xf5 -> push cpu AF
    0xc5 -> push cpu BC
    0xd5 -> push cpu DE
    0xe5 -> push cpu HL
    0xf1 -> pop cpu AF
    0xc1 -> pop cpu BC
    0xd1 -> pop cpu DE
    0xe1 -> pop cpu HL

    0x87 -> add cpu (Reg8 A)
    0x80 -> add cpu (Reg8 B)
    0x81 -> add cpu (Reg8 C)
    0x82 -> add cpu (Reg8 D)
    0x83 -> add cpu (Reg8 E)
    0x84 -> add cpu (Reg8 H)
    0x85 -> add cpu (Reg8 L)
    0x86 -> add cpu P_HL
    0xc6 -> add cpu N

    0x8f -> adc cpu (Reg8 A)
    0x88 -> adc cpu (Reg8 B)
    0x89 -> adc cpu (Reg8 C)
    0x8a -> adc cpu (Reg8 D)
    0x8b -> adc cpu (Reg8 E)
    0x8c -> adc cpu (Reg8 H)
    0x8d -> adc cpu (Reg8 L)
    0x8e -> adc cpu P_HL
    0xce -> adc cpu N

    0x97 -> sub cpu (Reg8 A)
    0x90 -> sub cpu (Reg8 B)
    0x91 -> sub cpu (Reg8 C)
    0x92 -> sub cpu (Reg8 D)
    0x93 -> sub cpu (Reg8 E)
    0x94 -> sub cpu (Reg8 H)
    0x95 -> sub cpu (Reg8 L)
    0x96 -> sub cpu P_HL
    0xd6 -> sub cpu N

    0x9f -> sbc cpu (Reg8 A)
    0x98 -> sbc cpu (Reg8 B)
    0x99 -> sbc cpu (Reg8 C)
    0x9a -> sbc cpu (Reg8 D)
    0x9b -> sbc cpu (Reg8 E)
    0x9c -> sbc cpu (Reg8 H)
    0x9d -> sbc cpu (Reg8 L)
    0x9e -> sbc cpu P_HL
    0xde -> sbc cpu N

    0xa7 -> and cpu (Reg8 A)
    0xa0 -> and cpu (Reg8 B)
    0xa1 -> and cpu (Reg8 C)
    0xa2 -> and cpu (Reg8 D)
    0xa3 -> and cpu (Reg8 E)
    0xa4 -> and cpu (Reg8 H)
    0xa5 -> and cpu (Reg8 L)
    0xa6 -> and cpu P_HL
    0xe6 -> and cpu N

    0xb7 -> or cpu (Reg8 A)
    0xb0 -> or cpu (Reg8 B)
    0xb1 -> or cpu (Reg8 C)
    0xb2 -> or cpu (Reg8 D)
    0xb3 -> or cpu (Reg8 E)
    0xb4 -> or cpu (Reg8 H)
    0xb5 -> or cpu (Reg8 L)
    0xb6 -> or cpu P_HL
    0xf6 -> or cpu N

    0xaf -> xor cpu (Reg8 A)
    0xa8 -> xor cpu (Reg8 B)
    0xa9 -> xor cpu (Reg8 C)
    0xaa -> xor cpu (Reg8 D)
    0xab -> xor cpu (Reg8 E)
    0xac -> xor cpu (Reg8 H)
    0xad -> xor cpu (Reg8 L)
    0xae -> xor cpu P_HL
    0xee -> xor cpu N

    0xbf -> cp cpu (Reg8 A)
    0xb8 -> cp cpu (Reg8 B)
    0xb9 -> cp cpu (Reg8 C)
    0xba -> cp cpu (Reg8 D)
    0xbb -> cp cpu (Reg8 E)
    0xbc -> cp cpu (Reg8 H)
    0xbd -> cp cpu (Reg8 L)
    0xbe -> cp cpu P_HL
    0xfe -> cp cpu N

    0x3c -> inc8 cpu (Reg8 A)
    0x04 -> inc8 cpu (Reg8 B)
    0x0c -> inc8 cpu (Reg8 C)
    0x14 -> inc8 cpu (Reg8 D)
    0x1c -> inc8 cpu (Reg8 E)
    0x24 -> inc8 cpu (Reg8 H)
    0x2c -> inc8 cpu (Reg8 L)
    0x34 -> inc8 cpu P_HL

    0x3d -> dec8 cpu (Reg8 A)
    0x05 -> dec8 cpu (Reg8 B)
    0x0d -> dec8 cpu (Reg8 C)
    0x15 -> dec8 cpu (Reg8 D)
    0x1d -> dec8 cpu (Reg8 E)
    0x25 -> dec8 cpu (Reg8 H)
    0x2d -> dec8 cpu (Reg8 L)
    0x35 -> dec8 cpu P_HL

    0x09 -> add_hl cpu BC
    0x19 -> add_hl cpu DE
    0x29 -> add_hl cpu HL
    0x39 -> add_hl cpu (Reg16 SP)
    0xe8 -> add_sp_n cpu 

    0x03 -> inc16 cpu BC
    0x13 -> inc16 cpu DE
    0x23 -> inc16 cpu HL
    0x33 -> inc16 cpu (Reg16 SP)

    0x0b -> dec16 cpu BC
    0x1b -> dec16 cpu DE
    0x2b -> dec16 cpu HL
    0x3b -> dec16 cpu (Reg16 SP)

    0x07 -> rlc cpu A_
    0x17 -> rl cpu A_
    0x0f -> rrc cpu A_
    0x1f -> rr cpu A_

    0x27 -> daa cpu 
    0x2f -> cpl cpu 
    0x3f -> ccf cpu 
    0x37 -> scf cpu 
    0xf3 -> di cpu 
    0xfb -> ei cpu 
    0x76 -> halt cpu 
    0x00 -> nop cpu 

    0xc3 -> jp cpu Always
    0xc2 -> jp cpu NotZero
    0xca -> jp cpu IsZero
    0xd2 -> jp cpu NotCarry
    0xda -> jp cpu IsCarry
    0xe9 -> jp_p_hl cpu 
    0x18 -> jr cpu Always
    0x20 -> jr cpu NotZero
    0x28 -> jr cpu IsZero
    0x30 -> jr cpu NotCarry
    0x38 -> jr cpu IsCarry
    0xcd -> call cpu Always
    0xc4 -> call cpu NotZero
    0xcc -> call cpu IsZero
    0xd4 -> call cpu NotCarry
    0xdc -> call cpu IsCarry
    0xc7 -> rst cpu 0x00
    0xcf -> rst cpu 0x08
    0xd7 -> rst cpu 0x10
    0xdf -> rst cpu 0x18
    0xe7 -> rst cpu 0x20
    0xef -> rst cpu 0x28
    0xf7 -> rst cpu 0x30
    0xff -> rst cpu 0x38
    0xc9 -> ret cpu Always
    0xc0 -> ret cpu NotZero
    0xc8 -> ret cpu IsZero
    0xd0 -> ret cpu NotCarry
    0xd8 -> ret cpu IsCarry
    0xd9 -> reti cpu 

    0x10 -> do
      code10 <- fetch8 cpu
      case code10 of
        0x00 -> stop cpu 
        _ -> error $ "CPU.execute: undefined instruction 0x10 " ++ showHex code10

    0xcb -> do
      codeCB <- fetch8 cpu
      case codeCB of
        0x37 -> swap cpu (Reg8 A)
        0x30 -> swap cpu (Reg8 B)
        0x31 -> swap cpu (Reg8 C)
        0x32 -> swap cpu (Reg8 D)
        0x33 -> swap cpu (Reg8 E)
        0x34 -> swap cpu (Reg8 H)
        0x35 -> swap cpu (Reg8 L)
        0x36 -> swap cpu P_HL

        0x07 -> rlc cpu (Reg8 A)
        0x00 -> rlc cpu (Reg8 B)
        0x01 -> rlc cpu (Reg8 C)
        0x02 -> rlc cpu (Reg8 D)
        0x03 -> rlc cpu (Reg8 E)
        0x04 -> rlc cpu (Reg8 H)
        0x05 -> rlc cpu (Reg8 L)
        0x06 -> rlc cpu P_HL

        0x17 -> rl cpu (Reg8 A)
        0x10 -> rl cpu (Reg8 B)
        0x11 -> rl cpu (Reg8 C)
        0x12 -> rl cpu (Reg8 D)
        0x13 -> rl cpu (Reg8 E)
        0x14 -> rl cpu (Reg8 H)
        0x15 -> rl cpu (Reg8 L)
        0x16 -> rl cpu P_HL

        0x0f -> rrc cpu (Reg8 A)
        0x08 -> rrc cpu (Reg8 B)
        0x09 -> rrc cpu (Reg8 C)
        0x0a -> rrc cpu (Reg8 D)
        0x0b -> rrc cpu (Reg8 E)
        0x0c -> rrc cpu (Reg8 H)
        0x0d -> rrc cpu (Reg8 L)
        0x0e -> rrc cpu P_HL

        0x1f -> rr cpu (Reg8 A)
        0x18 -> rr cpu (Reg8 B)
        0x19 -> rr cpu (Reg8 C)
        0x1a -> rr cpu (Reg8 D)
        0x1b -> rr cpu (Reg8 E)
        0x1c -> rr cpu (Reg8 H)
        0x1d -> rr cpu (Reg8 L)
        0x1e -> rr cpu P_HL

        0x27 -> sla cpu (Reg8 A)
        0x20 -> sla cpu (Reg8 B)
        0x21 -> sla cpu (Reg8 C)
        0x22 -> sla cpu (Reg8 D)
        0x23 -> sla cpu (Reg8 E)
        0x24 -> sla cpu (Reg8 H)
        0x25 -> sla cpu (Reg8 L)
        0x26 -> sla cpu P_HL

        0x2f -> sra cpu (Reg8 A)
        0x28 -> sra cpu (Reg8 B)
        0x29 -> sra cpu (Reg8 C)
        0x2a -> sra cpu (Reg8 D)
        0x2b -> sra cpu (Reg8 E)
        0x2c -> sra cpu (Reg8 H)
        0x2d -> sra cpu (Reg8 L)
        0x2e -> sra cpu P_HL

        0x3f -> srl cpu (Reg8 A)
        0x38 -> srl cpu (Reg8 B)
        0x39 -> srl cpu (Reg8 C)
        0x3a -> srl cpu (Reg8 D)
        0x3b -> srl cpu (Reg8 E)
        0x3c -> srl cpu (Reg8 H)
        0x3d -> srl cpu (Reg8 L)
        0x3e -> srl cpu P_HL

        0x47 -> bit cpu 0 (Reg8 A)
        0x40 -> bit cpu 0 (Reg8 B)
        0x41 -> bit cpu 0 (Reg8 C)
        0x42 -> bit cpu 0 (Reg8 D)
        0x43 -> bit cpu 0 (Reg8 E)
        0x44 -> bit cpu 0 (Reg8 H)
        0x45 -> bit cpu 0 (Reg8 L)
        0x46 -> bit cpu 0 P_HL
        0x4f -> bit cpu 1 (Reg8 A)
        0x48 -> bit cpu 1 (Reg8 B)
        0x49 -> bit cpu 1 (Reg8 C)
        0x4a -> bit cpu 1 (Reg8 D)
        0x4b -> bit cpu 1 (Reg8 E)
        0x4c -> bit cpu 1 (Reg8 H)
        0x4d -> bit cpu 1 (Reg8 L)
        0x4e -> bit cpu 1 P_HL
        0x57 -> bit cpu 2 (Reg8 A)
        0x50 -> bit cpu 2 (Reg8 B)
        0x51 -> bit cpu 2 (Reg8 C)
        0x52 -> bit cpu 2 (Reg8 D)
        0x53 -> bit cpu 2 (Reg8 E)
        0x54 -> bit cpu 2 (Reg8 H)
        0x55 -> bit cpu 2 (Reg8 L)
        0x56 -> bit cpu 2 P_HL
        0x5f -> bit cpu 3 (Reg8 A)
        0x58 -> bit cpu 3 (Reg8 B)
        0x59 -> bit cpu 3 (Reg8 C)
        0x5a -> bit cpu 3 (Reg8 D)
        0x5b -> bit cpu 3 (Reg8 E)
        0x5c -> bit cpu 3 (Reg8 H)
        0x5d -> bit cpu 3 (Reg8 L)
        0x5e -> bit cpu 3 P_HL
        0x67 -> bit cpu 4 (Reg8 A)
        0x60 -> bit cpu 4 (Reg8 B)
        0x61 -> bit cpu 4 (Reg8 C)
        0x62 -> bit cpu 4 (Reg8 D)
        0x63 -> bit cpu 4 (Reg8 E)
        0x64 -> bit cpu 4 (Reg8 H)
        0x65 -> bit cpu 4 (Reg8 L)
        0x66 -> bit cpu 4 P_HL
        0x6f -> bit cpu 5 (Reg8 A)
        0x68 -> bit cpu 5 (Reg8 B)
        0x69 -> bit cpu 5 (Reg8 C)
        0x6a -> bit cpu 5 (Reg8 D)
        0x6b -> bit cpu 5 (Reg8 E)
        0x6c -> bit cpu 5 (Reg8 H)
        0x6d -> bit cpu 5 (Reg8 L)
        0x6e -> bit cpu 5 P_HL
        0x77 -> bit cpu 6 (Reg8 A)
        0x70 -> bit cpu 6 (Reg8 B)
        0x71 -> bit cpu 6 (Reg8 C)
        0x72 -> bit cpu 6 (Reg8 D)
        0x73 -> bit cpu 6 (Reg8 E)
        0x74 -> bit cpu 6 (Reg8 H)
        0x75 -> bit cpu 6 (Reg8 L)
        0x76 -> bit cpu 6 P_HL
        0x7f -> bit cpu 7 (Reg8 A)
        0x78 -> bit cpu 7 (Reg8 B)
        0x79 -> bit cpu 7 (Reg8 C)
        0x7a -> bit cpu 7 (Reg8 D)
        0x7b -> bit cpu 7 (Reg8 E)
        0x7c -> bit cpu 7 (Reg8 H)
        0x7d -> bit cpu 7 (Reg8 L)
        0x7e -> bit cpu 7 P_HL

        0xc7 -> set cpu 0 (Reg8 A)
        0xc0 -> set cpu 0 (Reg8 B)
        0xc1 -> set cpu 0 (Reg8 C)
        0xc2 -> set cpu 0 (Reg8 D)
        0xc3 -> set cpu 0 (Reg8 E)
        0xc4 -> set cpu 0 (Reg8 H)
        0xc5 -> set cpu 0 (Reg8 L)
        0xc6 -> set cpu 0 P_HL
        0xcf -> set cpu 1 (Reg8 A)
        0xc8 -> set cpu 1 (Reg8 B)
        0xc9 -> set cpu 1 (Reg8 C)
        0xca -> set cpu 1 (Reg8 D)
        0xcb -> set cpu 1 (Reg8 E)
        0xcc -> set cpu 1 (Reg8 H)
        0xcd -> set cpu 1 (Reg8 L)
        0xce -> set cpu 1 P_HL
        0xd7 -> set cpu 2 (Reg8 A)
        0xd0 -> set cpu 2 (Reg8 B)
        0xd1 -> set cpu 2 (Reg8 C)
        0xd2 -> set cpu 2 (Reg8 D)
        0xd3 -> set cpu 2 (Reg8 E)
        0xd4 -> set cpu 2 (Reg8 H)
        0xd5 -> set cpu 2 (Reg8 L)
        0xd6 -> set cpu 2 P_HL
        0xdf -> set cpu 3 (Reg8 A)
        0xd8 -> set cpu 3 (Reg8 B)
        0xd9 -> set cpu 3 (Reg8 C)
        0xda -> set cpu 3 (Reg8 D)
        0xdb -> set cpu 3 (Reg8 E)
        0xdc -> set cpu 3 (Reg8 H)
        0xdd -> set cpu 3 (Reg8 L)
        0xde -> set cpu 3 P_HL
        0xe7 -> set cpu 4 (Reg8 A)
        0xe0 -> set cpu 4 (Reg8 B)
        0xe1 -> set cpu 4 (Reg8 C)
        0xe2 -> set cpu 4 (Reg8 D)
        0xe3 -> set cpu 4 (Reg8 E)
        0xe4 -> set cpu 4 (Reg8 H)
        0xe5 -> set cpu 4 (Reg8 L)
        0xe6 -> set cpu 4 P_HL
        0xef -> set cpu 5 (Reg8 A)
        0xe8 -> set cpu 5 (Reg8 B)
        0xe9 -> set cpu 5 (Reg8 C)
        0xea -> set cpu 5 (Reg8 D)
        0xeb -> set cpu 5 (Reg8 E)
        0xec -> set cpu 5 (Reg8 H)
        0xed -> set cpu 5 (Reg8 L)
        0xee -> set cpu 5 P_HL
        0xf7 -> set cpu 6 (Reg8 A)
        0xf0 -> set cpu 6 (Reg8 B)
        0xf1 -> set cpu 6 (Reg8 C)
        0xf2 -> set cpu 6 (Reg8 D)
        0xf3 -> set cpu 6 (Reg8 E)
        0xf4 -> set cpu 6 (Reg8 H)
        0xf5 -> set cpu 6 (Reg8 L)
        0xf6 -> set cpu 6 P_HL
        0xff -> set cpu 7 (Reg8 A)
        0xf8 -> set cpu 7 (Reg8 B)
        0xf9 -> set cpu 7 (Reg8 C)
        0xfa -> set cpu 7 (Reg8 D)
        0xfb -> set cpu 7 (Reg8 E)
        0xfc -> set cpu 7 (Reg8 H)
        0xfd -> set cpu 7 (Reg8 L)
        0xfe -> set cpu 7 P_HL

        0x87 -> res cpu 0 (Reg8 A)
        0x80 -> res cpu 0 (Reg8 B)
        0x81 -> res cpu 0 (Reg8 C)
        0x82 -> res cpu 0 (Reg8 D)
        0x83 -> res cpu 0 (Reg8 E)
        0x84 -> res cpu 0 (Reg8 H)
        0x85 -> res cpu 0 (Reg8 L)
        0x86 -> res cpu 0 P_HL
        0x8f -> res cpu 1 (Reg8 A)
        0x88 -> res cpu 1 (Reg8 B)
        0x89 -> res cpu 1 (Reg8 C)
        0x8a -> res cpu 1 (Reg8 D)
        0x8b -> res cpu 1 (Reg8 E)
        0x8c -> res cpu 1 (Reg8 H)
        0x8d -> res cpu 1 (Reg8 L)
        0x8e -> res cpu 1 P_HL
        0x97 -> res cpu 2 (Reg8 A)
        0x90 -> res cpu 2 (Reg8 B)
        0x91 -> res cpu 2 (Reg8 C)
        0x92 -> res cpu 2 (Reg8 D)
        0x93 -> res cpu 2 (Reg8 E)
        0x94 -> res cpu 2 (Reg8 H)
        0x95 -> res cpu 2 (Reg8 L)
        0x96 -> res cpu 2 P_HL
        0x9f -> res cpu 3 (Reg8 A)
        0x98 -> res cpu 3 (Reg8 B)
        0x99 -> res cpu 3 (Reg8 C)
        0x9a -> res cpu 3 (Reg8 D)
        0x9b -> res cpu 3 (Reg8 E)
        0x9c -> res cpu 3 (Reg8 H)
        0x9d -> res cpu 3 (Reg8 L)
        0x9e -> res cpu 3 P_HL
        0xa7 -> res cpu 4 (Reg8 A)
        0xa0 -> res cpu 4 (Reg8 B)
        0xa1 -> res cpu 4 (Reg8 C)
        0xa2 -> res cpu 4 (Reg8 D)
        0xa3 -> res cpu 4 (Reg8 E)
        0xa4 -> res cpu 4 (Reg8 H)
        0xa5 -> res cpu 4 (Reg8 L)
        0xa6 -> res cpu 4 P_HL
        0xaf -> res cpu 5 (Reg8 A)
        0xa8 -> res cpu 5 (Reg8 B)
        0xa9 -> res cpu 5 (Reg8 C)
        0xaa -> res cpu 5 (Reg8 D)
        0xab -> res cpu 5 (Reg8 E)
        0xac -> res cpu 5 (Reg8 H)
        0xad -> res cpu 5 (Reg8 L)
        0xae -> res cpu 5 P_HL
        0xb7 -> res cpu 6 (Reg8 A)
        0xb0 -> res cpu 6 (Reg8 B)
        0xb1 -> res cpu 6 (Reg8 C)
        0xb2 -> res cpu 6 (Reg8 D)
        0xb3 -> res cpu 6 (Reg8 E)
        0xb4 -> res cpu 6 (Reg8 H)
        0xb5 -> res cpu 6 (Reg8 L)
        0xb6 -> res cpu 6 P_HL
        0xbf -> res cpu 7 (Reg8 A)
        0xb8 -> res cpu 7 (Reg8 B)
        0xb9 -> res cpu 7 (Reg8 C)
        0xba -> res cpu 7 (Reg8 D)
        0xbb -> res cpu 7 (Reg8 E)
        0xbc -> res cpu 7 (Reg8 H)
        0xbd -> res cpu 7 (Reg8 L)
        0xbe -> res cpu 7 P_HL
        _ -> error $  "CPU.execute: undefined instruction 0xcb " ++ showHex codeCB

    _ -> error $ "CPU.execute: undefined instruction " ++ showHex code

serial :: CPU -> IO ()
serial cpu@(CPU {..}) = do
  sc <- readGBReg mbc SC
  when (testBit sc 7) $ do
    let 
      ls = [512, 256, 16, 8] :: [Word64]
      clock = ls !! fi (sc .&. 0b11)
    sys <- readReg64 cpu SysCounter
    when (sys `mod` clock == 0) $ do
      sb <- readGBReg mbc SB
      writeLogger serialLogger sb
      writeGBReg mbc SC $ clearBit sc 7
      modifyGBReg mbc IF $ flip setBit 3

timer :: CPU -> IO ()
timer cpu@(CPU {..}) = do
  sys <- readReg64 cpu SysCounter
  when (sys `mod` 256 == 0) $ do
    modifyGBReg mbc DIV (+ 1)

  tac <- readGBReg mbc TAC
  when (testBit tac 2) $ do
    let 
      ls = [1024, 16, 64, 256] :: [Word64]
      clock = ls !! fi (tac .&. 0b11)
    when (sys `mod` clock == 0) $ do
      tima <- readGBReg mbc TIMA
      let (tima', carry, _) = addCarryHalf tima (1 :: Word8)
      if carry then do
        tma <- readGBReg mbc TMA
        modifyGBReg mbc IF $ flip setBit 2
        writeGBReg mbc TIMA tma
      else do
        writeGBReg mbc TIMA tima'

joypad :: CPU -> IO ()
joypad (CPU {..}) = do
  jb <- readStore joypadBuffer 0
  joyp <- readGBReg mbc JOYP
  when (not $ testBit joyp 4) $ do
    writeGBReg mbc JOYP (0b100000 .|. jb .&. 0b1111)
    modifyGBReg mbc IF $ flip setBit 4
  when (not $ testBit joyp 5) $ do
    writeGBReg mbc JOYP (0b010000 .|. jb `shiftR` 4)
    modifyGBReg mbc IF $ flip setBit 4
    
interrupt :: CPU -> IO ()
interrupt cpu@(CPU {..}) = do
  enable <- readGBReg mbc IE
  request <- readGBReg mbc IF
  when (enable .&. request /= 0) $ do
    writeReg8 cpu Halt 0

  ime <- readReg8 cpu IME
  when (ime == 1) $ do
    writeReg8 cpu Halt 0
    let
      (addr, n, _ :: String) = 
        if testBit enable 0 && testBit request 0 then (0x40, 0, "VBlank")
        else if testBit enable 1 && testBit request 1 then (0x48, 1, "LSTAT")
        else if testBit enable 2 && testBit request 2 then (0x50, 2, "Timer")
        else if testBit enable 3 && testBit request 3 then (0x58, 3, "Serial")
        else if testBit enable 4 && testBit request 4 then (0x60, 4, "Joypad")
        else (0, 0, "")
    when (addr /= 0) $ do
      pc <- readReg16 cpu PC
      push16 cpu pc
      writeReg16 cpu PC addr
      writeReg8 cpu IME 0
      writeReg8 cpu Halt 0
      modifyGBReg mbc IF $ flip clearBit n
      tick cpu
      tick cpu
      tick cpu

stepCPU :: CPU -> IO ()
stepCPU cpu = do
  writeReg8 cpu Cycle 0

  halting <- readReg8 cpu Halt
  if halting == 1 then
    --nop cpu
    tick cpu
  else do
    executeInstruction cpu
    modifyReg64 cpu ExeCounter (+ 1)

  cycle <- readReg8 cpu Cycle
  replicateM_ (fi cycle * 4) $ do
    serial cpu
    timer cpu
    joypad cpu
    interrupt cpu
    modifyReg64 cpu SysCounter (+ 1)
