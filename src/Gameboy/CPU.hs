module Gameboy.CPU where

import Prelude hiding (read, cycle, log, or, and)

import Gameboy.Internal
import Gameboy.MBC
import Gameboy.Logger
import Gameboy.Utils hiding (set,bit,xor,modify)

import qualified Numeric as N
import qualified Data.Bits as B
import qualified Data.Vector as V

data OP
  = A | F | B | C | D | E | H | L
  | AF | BC | DE | HL | SP 
  | P_BC | P_DE | P_HL | P_WW
  | P_FF00_C | P_FF00_W
  | W | WW
  | Zero | NotZero | Carry | NotCarry | Always
  deriving Show

data CPULogInfo 
  = Hex8 Word8 | Hex16 Word16 | Signed8 Word8 
  | Id String | Info String | Rst String 
  | Non 

instance Show CPULogInfo where
  show Non = ""
  show (Id s) = s
  show (Info s) = "# " ++ s
  show (Rst s) = "-> " ++ s
  show (Hex8 w) = showHex w
  show (Hex16 ww) = showHex ww
  show (Signed8 w) = showSignedWord8 w


newCPUState :: CPUState
newCPUState = CPUState {
  _a = 0,
  __f = 0, -- 0xb0,
  _b = 0x13,
  _c = 0,
  _d = 0xd8,
  _e = 0,
  _h = 0x01,
  _l = 0x4d,
  _sp = 0xfffe,
  _pc = 0x100,
  _serial_counter = 0,
  _serial_buffer = V.empty,
  _sys_counter = 0,
  _ime = False,
  --_ime_prev = False,
  _halting = False,
  _stoping = False,
  _cycle = 0,
  _cycleM = 0,
  _exe_counter = 1
  }

f :: Lens' CPUState Word8
f = lens get set
  where
    get = __f
    set c w = c { __f = w .&. 0xf0 } -- ignore/reset lower 4 bits always

carry :: Lens' CPUState Bool
carry = lens get set
  where
    get c = testBit (__f c) 4
    set c b =
      if b then
        c { __f = setBit (__f c) 4 }
      else
        c { __f = clearBit (__f c) 4 }

half :: Lens' CPUState Bool
half = lens get set
  where
    get c = testBit (__f c) 5
    set c b =
      if b then
        c { __f = setBit (__f c) 5 }
      else
        c { __f = clearBit (__f c) 5 }

negative :: Lens' CPUState Bool
negative = lens get set
  where
    get c = testBit (__f c) 6
    set c b =
      if b then
        c { __f = setBit (__f c) 6 }
      else
        c { __f = clearBit (__f c) 6 }

zero :: Lens' CPUState Bool
zero = lens get set
  where
    get c = testBit (__f c) 7
    set c b =
      if b then
        c { __f = setBit (__f c) 7 }
      else
        c { __f = clearBit (__f c) 7 }


af :: Lens' CPUState Word16
af = lens get set
  where
    get c = toWW (_a c) (__f c)
    set c ww = c { _a = _a', __f = __f'' }
      where
        (_a',__f') = sepWW ww
        __f'' = __f' .&. 0xf0

bc :: Lens' CPUState Word16
bc = lens get set
  where
    get x = toWW (_b x) (_c x)
    set x ww = x { _b = _b', _c = _c' }
      where
        (_b',_c') = sepWW ww

de :: Lens' CPUState Word16
de = lens get set
  where
    get c = toWW (_d c) (_e c)
    set c ww = c { _d = _d', _e = _e' }
      where
        (_d',_e') = sepWW ww

hl :: Lens' CPUState Word16
hl = lens get set
  where
    get c = toWW (_h c) (_l c)
    set c ww = c { _h = _h', _l = _l' }
      where
        (_h',_l') = sepWW ww



read :: Address i => i -> GB Word8
read i = do
  r <- use $ mbc.reader
  a <- r $ toInt i
  logging 1 ("Read: " ++ showHex (toInt i) ++ " -> " ++ showHex a)
  pure a


read' :: Address i => i -> GB Word8
read' i = do
  r <- use $ mbc.reader
  a <- r $ toInt i
  pure a

write :: Address i => i -> Word8 -> GB ()
write i w = do
  wr <- use $ mbc.writer
  logging 1 ("Writ: " ++ showHex (toInt i) ++ " <- " ++ showHex w)
  wr (toInt i) w

modify :: Address i => i -> (Word8 -> Word8) -> GB ()
modify i f = do
  x <- read i
  write i $ f x

log :: Int -> String -> GB ()
log n s = logging n s

push' :: Word8 -> GB ()
push' i = do
  cpu.sp -= 1
  sp' <- use $ cpu.sp 
  write sp' i

pop' :: GB Word8
pop' = do
  sp' <- use $ cpu.sp
  w <- read sp'
  cpu.sp += 1
  pure w


showCPUState :: CPUState -> String
showCPUState cpu =
--  "pc:" ++ showHex' (cpu^.pc)
--    ++ " sp:" ++ showHex' (cpu^.sp)
--    ++ " a:" ++ showHex' (cpu^.a)
    "A:" ++ showHex' (cpu^.a)
    ++ " F:" ++ showHex' (cpu^.f)
    ++ " B:" ++ showHex' (cpu^.b)
    ++ " C:" ++ showHex' (cpu^.c)
    ++ " D:" ++ showHex' (cpu^.d)
    ++ " E:" ++ showHex' (cpu^.e)
    ++ " H:" ++ showHex' (cpu^.h)
    ++ " L:" ++ showHex' (cpu^.l)
    ++ "\n"
    ++ "Z:" ++ show (cpu^.zero&toNum)
    ++ " N:" ++ show (cpu^.negative&toNum)
    ++ " H:" ++ show (cpu^.half&toNum)
    ++ " C:" ++ show (cpu^.carry&toNum)
    ++ " IME:" ++ show (cpu^.ime&toNum)
    ++ " HALT:" ++ show (cpu^.halting&toNum)
    ++ " SB:" ++ (cpu^.serial_buffer & V.map (chr.fi) & V.toList & show)


executeCPU :: GB ()
executeCPU = do
  cpu.cycleM .= 0

  gb <- get
  (Just bank') <- preuse $ mbc.mbcnState.bank
  e <- use $ cpu.exe_counter
  p <- use $ cpu.pc
  s <- use $ cpu.sp
  pss <- mapM (read.(p +)) [0..0x14]
  spl <- mapM (read.(s -)) $ reverse [1..4]
  sp' <- read s
  spr <- mapM (read.(s +)) [1..0x0f]
  let
    sp'' = "|" ++ showHex' sp' ++ "| "
    cms = concatMap ((++ " ").showHex')
    str =
      ("--- " ++ show e) ++ "\n"
      ++ ("BANK: " ++ showHex bank') ++ "\n"
      ++ ("PC:" ++ showHex16 p ++ " " ++ cms pss) ++ "\n"
      ++ ("SP:" ++ showHex16 s ++ " " ++ cms spl ++ sp'' ++ cms spr) ++ "\n"
      ++ (showCPUState $ gb^.cpu)
  log 3 str

  halt <- use $ cpu.halting
  if halt then
    nop
  else
    dispatch

  cycle <- use $ cpu.cycleM
  forM_ [1 .. cycle] $ \i -> do
    serial
    timer
    interrupt
    cpu.sys_counter += 1

  cpu.exe_counter += 1



timer :: GB ()
timer = do
  ssc <- use $ cpu.sys_counter

  when (ssc `mod` 256 == 0) $ do
    modify DIV (+ 1)

  tac <- read TAC
  when (testBit tac 2) $ do
    let clock = [1024,16,64,256] !! fi (tac .&. 0b11)
    when (ssc `mod` clock == 0) $ do
      tima <- read TIMA
      let (tima',carry',_) = add8_IsCarryHalf tima 1
      if carry' then do
        modify IF (.|. 0b100)
        tma <- read TMA
        write TIMA tma
        cpu.halting .= False
        log 4 ("Timer: clock:" ++ show clock ++ " tma:" ++ showHex tma)
      else
        write TIMA tima'

serial :: GB ()
serial = do
  sc <- read SC
  when (testBit sc 7) $ do
    let clock = [512, 256, 16, 8] !! fi (sc .&. 0b11) -- 8192, 16384, 262144, 524288
    ssc <- use $ cpu.sys_counter
    when (ssc `mod` clock == 0) $ do
      sb <- read SB
      cpu.serial_buffer %= (flip V.snoc sb)
      sbs <- use $ cpu.serial_buffer
      log 4 ("Serial: " ++ (V.toList $ V.map (chr.fi) sbs))

      write SC $ clearBit sc 7
      modify IF (.|. 0b1000)

interrupt :: GB ()
interrupt = do
  master <- use $ cpu.ime
  --ime_prev .= master
  when master $ do
    enable <- read IE
    request <- read IF
    let 
      (addr, bit, cate) = 
        if testBit enable 0 && testBit request 0 then
          (0x40, 0, "VBlack")
        else if testBit enable 1 && testBit request 1 then
          (0x48, 1, "LSTAT")
        else if testBit enable 2 && testBit request 2 then
          (0x50, 2, "Timer")
        else if testBit enable 3 && testBit request 3 then
          (0x58, 3, "Seria")
        else if testBit enable 4 && testBit request 4 then
          (0x60, 4, "Joypad")
        else
          (0, 0, "NOP")
    when (not (addr == 0 && bit == 0)) $ do
      pc' <- use $ cpu.pc
      let (h',l') = sepWW pc'
      push' h'
      push' l'
      --write IE $ clearBit enable bit
      write IF $ clearBit request bit
      cpu 
        %= (pc .~ addr) 
        . (ime .~ False) 
        . (halting .~ False) 
        . (cycleM %~ (+5))
      log 4 ("Interrupt: " ++ cate ++ " from " ++ showHex pc')


add8_IsCarryHalf :: Word8 -> Word8 -> (Word8, Bool, Bool)
add8_IsCarryHalf w1 w2 = (w3, w1 > w3, w4 > 0xf)
  where
    w3 = w1 + w2
    w4 = (w1 .&. 0xf) + (w2 .&. 0xf)

sub8_IsCarryHalf :: Word8 -> Word8 -> (Word8, Bool, Bool)
sub8_IsCarryHalf w1 w2 = (w3, w1 < w3, w1 < w4)
  where
    w3 = w1 - w2
    w4 = (w1 .&. 0xf) - (w2 .&. 0xf)

add16_IsCarryHalf :: Word16 -> Word16 -> (Word16, Bool, Bool)
add16_IsCarryHalf w1 w2 = (w3, w1 > w3, w4 > 0x7ff)
  where
    w3 = w1 + w2 
    w4 = (w1 .&. 0x7ff) + (w2 .&. 0x7ff)

sub16_IsCarryHalf :: Word16 -> Word16 -> (Word16, Bool, Bool)
sub16_IsCarryHalf w1 w2 = (w3, w1 < w3, w1 < w4)
  where
    w3 = w1 - w2
    w4 = (w1 .&. 0x7ff) - (w2 .&. 0x7ff)

add_Word16_SignedWord8_IsCarryHalf :: Word16 -> Word8 -> (Word16, Bool, Bool)
add_Word16_SignedWord8_IsCarryHalf w i =
  let
    i' :: Word16
    i' = fi $ clearBit i 7
  in
  if testBit i 7 then
    let 
      w' = w - (128 - i')
      c' = (w .&. 0xff) - (128 - i')
      h' = (w' .&. 0xf) - (i' .&. 0xf)
    in (w', c' > 0xff, h' > 0xf)
  else
    let 
      w' = w + i'
      c' = (w .&. 0xff) + i'
      h' = (w' .&. 0xf) + (i' .&. 0xf)
    in (w', c' > 0xff, h' > 0xf)
  

readPC :: GB Word8
readPC = do
  pc' <- use $ cpu.pc
  w <- read pc'
  cpu.pc += 1
  pure w

readOP8 :: OP -> GB Word8
readOP8 op = do
  case op of
    A -> use $ cpu.a
    F -> use $ cpu.f
    B -> use $ cpu.b
    C -> use $ cpu.c
    D -> use $ cpu.d
    E -> use $ cpu.e
    H -> use $ cpu.h
    L -> use $ cpu.l
    W ->  cpu.cycleM += 1 >> readPC
    P_BC -> cpu.cycleM += 1 >> (use $ cpu.bc) >>= read
    P_DE -> cpu.cycleM += 1 >> (use $ cpu.de) >>= read
    P_HL -> cpu.cycleM += 1 >> (use $ cpu.hl) >>= read
    P_WW -> cpu.cycleM += 3 >> (flip toWW <$> readPC <*> readPC) >>= read . toInt
    P_FF00_C -> cpu.cycleM += 1 >> (use $ cpu.c) >>= read . (0xff00 +) . toInt
    P_FF00_W -> cpu.cycleM += 2 >> readPC >>= read . (0xff00 +) . toInt

writeOP8 :: OP -> Word8 -> GB ()
writeOP8 op w = do
  case op of
    A -> cpu.a .= w
    F -> cpu.f .= w
    B -> cpu.b .= w
    C -> cpu.c .= w
    D -> cpu.d .= w
    E -> cpu.e .= w
    H -> cpu.h .= w
    L -> cpu.l .= w
    P_BC -> cpu.cycleM += 1 >> (use $ cpu.bc) >>= flip write w
    P_DE -> cpu.cycleM += 1 >> (use $ cpu.de) >>= flip write w
    P_HL -> cpu.cycleM += 1 >> (use $ cpu.hl) >>= flip write w
    P_WW -> cpu.cycleM += 3 >> (flip toWW <$> readPC <*> readPC) >>= flip write w
    P_FF00_C -> cpu.cycleM += 1 >> (use $ cpu.c) >>= flip write w . (0xff00 +) . toInt
    P_FF00_W -> cpu.cycleM += 2 >> readPC >>= flip write w . (0xff00 +) . toInt
    _ -> error $ show op

readOP16 :: OP -> GB Word16
readOP16 op = do
  case op of
    AF -> use $ cpu.af
    BC -> use $ cpu.bc
    DE -> use $ cpu.de
    HL -> use $ cpu.hl
    SP -> use $ cpu.sp
    WW -> cpu.cycleM += 2 >> (flip toWW <$> readPC <*> readPC)

writeOP16 :: OP -> Word16 -> GB ()
writeOP16 op ww = do
  case op of
    AF -> cpu.af .= ww
    BC -> cpu.bc .= ww
    DE -> cpu.de .= ww
    HL -> cpu.hl .= ww
    SP -> cpu.sp .= ww
    P_WW -> do 
      cpu.cycleM += 3
      addr <- flip toWW <$> readPC <*> readPC
      let (h', l') = sepWW ww
      write addr l'
      write (addr + 1) h'

logI :: (Show a, Show b, Show c) => String -> a -> b -> c -> GB ()
logI instr o1 o2 o3 = log 3 ("> " ++ instr ++ " " ++ show o1 ++ " " ++ show o2 ++ " " ++ show o3)

nop :: GB ()
nop = do
  cpu.cycleM += 1
  log 3 "NOP"

ld8 :: OP -> OP -> GB ()
ld8 op1 op2 = do
  w <- readOP8 op2
  writeOP8 op1 w

  cpu.cycleM += 1 
  logI "LD" op1 op2 (Rst $ showHex w)

ld16 :: OP -> OP -> GB ()
ld16 op1 op2 = do
  ww <- readOP16 op2
  writeOP16 op1 ww

  cpu.cycleM += 2 
  logI "LD" op1 op2 (Rst $ showHex ww)

ld8_id_a_p_hl :: (Word16 -> Word16) -> String -> GB ()
ld8_id_a_p_hl f s = do
  hl' <- use $ cpu.hl
  w <- read hl'

  cpu %= (a .~ w) 
    . (hl %~ f)
    . (cycleM %~ (+ 2))

  logI s A P_HL Non

ld8_id_p_hl_a :: (Word16 -> Word16) -> String -> GB ()
ld8_id_p_hl_a f s = do
  hl' <- use $ cpu.hl
  a' <- use $ cpu.a
  write hl' a'

  cpu %= (hl %~ f)
    . (cycleM %~ (+2))

  logI s P_HL A Non

ld16_hl_sp_w :: GB ()
ld16_hl_sp_w = do
  w <- readPC
  sp' <- use $ cpu.sp 
  let (sp'',c',h') = add_Word16_SignedWord8_IsCarryHalf sp' w

  cpu %= (hl .~ sp'')
    . (zero .~ False)
    . (negative .~ False)
    . (half .~ h')
    . (carry .~ c')
    . (cycleM %~ (+3))

  logI "LD" HL SP $ Signed8 w

push :: OP -> GB ()
push op = do
  ww <- readOP16 op
  let (h',l') = sepWW ww
  push' h'
  push' l'

  cpu.cycleM += 4
  logI "PUSH" op (showHex ww) Non

pop :: OP -> GB ()
pop op = do
  l' <- pop'
  h' <- pop'
  writeOP16 op $ toWW h' l'

  cpu.cycleM += 3
  logI "POP" op (showHex $ toWW h' l') Non

add :: OP -> GB ()
add op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  let (a'', c', h') = add8_IsCarryHalf a' w

  cpu %= (a .~ a'')
    . (zero .~ isZero a'')
    . (negative .~ False)
    . (half .~ h')
    . (carry .~ c')
    . (cycleM %~ (+1))

  logI "ADD" op (Rst $ showHex w) ""

adc :: OP -> GB ()
adc op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  c' <- use $ cpu.carry
  let 
    (a'', c'', h'') = add8_IsCarryHalf a' w
    (a''', c''', h''') = add8_IsCarryHalf a'' $ toNum c'

  cpu %= (a .~ a''')
    . (zero .~ isZero a''')
    . (negative .~ False)
    . (half .~ (h'' || h'''))
    . (carry .~ (c'' || c'''))
    . (cycleM %~ (+1))

  logI "ADC" op (Rst $ showHex w) Non

sub :: OP -> GB ()
sub op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  let (a'', c', h') = sub8_IsCarryHalf a' w

  cpu %= (a .~ a'')
    . (zero .~ isZero a'')
    . (negative .~ True)
    . (half .~ h')
    . (carry .~ c')
    . (cycleM %~ (+1))
 
  logI "SUB" op (Rst $ showHex w) Non

sbc :: OP -> GB ()
sbc op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  c' <- use $ cpu.carry
  let 
    (a'', c'', h'') = sub8_IsCarryHalf a' w
    (a''', c''', h''') = sub8_IsCarryHalf a'' $ toNum c'

  cpu %= (a .~ a''')
    . (zero .~ isZero a''')
    . (negative .~ True)
    . (half .~ (h'' || h'''))
    . (carry .~ (c'' || c'''))
    . (cycleM %~ (+1))

  logI "SBC" op (Rst $ showHex w) Non

and :: OP -> GB ()
and op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  let a'' = a' .&. w

  cpu %= (a .~ a'')
    . (zero .~ isZero a'')
    . (negative .~ False)
    . (half .~ True)
    . (carry .~ False)
    . (cycleM %~ (+1))

  logI "AND" op (Rst $ showHex w) Non

or :: OP -> GB ()
or op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  let a'' = a' .|. w

  cpu %= (a .~ a'')
    . (zero .~ isZero a'')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ False)
    . (cycleM %~ (+1))

  logI "OR" op (Rst $ showHex w) Non

xor :: OP -> GB ()
xor op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  let a'' = a' `B.xor` w

  cpu %= (a .~ a'')
    . (zero .~ isZero a'')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ False)
    . (cycleM %~ (+1))

  logI "XOR" op (Rst $ showHex w) Non

cp :: OP -> GB ()
cp op = do
  a' <- use $ cpu.a
  w <- readOP8 op
  let (a'', c', h') = sub8_IsCarryHalf a' w

  cpu %= (zero .~ isZero a'')
   . (negative .~ True)
   . (half .~ h')
   . (carry .~ c')
   . (cycleM %~ (+1))

  logI "CP" op (Rst $ showHex w) Non

inc8 :: OP -> GB ()
inc8 op = do
  w <- readOP8 op
  let (w', c', h') = add8_IsCarryHalf w 1
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ h')
    . (cycleM %~ (+1))

  logI "INC" op (Rst $ showHex w') Non

dec8 :: OP -> GB ()
dec8 op = do
  w <- readOP8 op
  let (w', c', h') = sub8_IsCarryHalf w 1
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ True)
    . (half .~ h')
    . (cycleM %~ (+1))

  logI "DEC" op (Rst $ showHex w') Non

add_hl :: OP -> GB ()
add_hl op = do
  hl' <- use $ cpu.hl
  ww <- readOP16 op
  let (hl'',carry',half') = add16_IsCarryHalf hl' ww

  cpu %= (hl .~ hl'')
    . (negative .~ False)
    . (half .~ half')
    . (carry .~ carry')
    . (cycleM %~ (+2))

  logI "ADD" HL op $ Rst $ showHex ww

add_sp :: GB ()
add_sp = do
  sp' <- use $ cpu.sp
  w <- readPC
  let (sp'', c', h') = add_Word16_SignedWord8_IsCarryHalf sp' w

  cpu %= (sp .~ sp'')
    . (zero .~ False)
    . (negative .~ False)
    . (half .~ h')
    . (carry .~ c')
    . (cycleM %~ (+2))

  logI "ADD" SP w $ showSignedWord8 w

inc16 :: OP -> GB ()
inc16 op = do
  ww <- readOP16 op
  let (ww', carry', _) = add16_IsCarryHalf ww 1
  writeOP16 op ww'
  
  cpu %= (carry .~ carry')
    . (cycleM %~ (+2))

  logI "INC" op (Rst $ showHex ww) Non

dec16 :: OP -> GB ()
dec16 op = do
  ww <- readOP16 op
  let (ww', carry', _) = sub16_IsCarryHalf ww 1
  writeOP16 op ww'

  cpu %= (carry .~ carry')
    . (cycleM %~ (+2))
  
  logI "DEC" op (Rst $ showHex ww) Non

daa :: GB ()
daa = do
  a' <- use $ cpu.a
  negative' <- use $ cpu.negative
  half' <- use $ cpu.half
  carry' <- use $ cpu.carry
  if not negative' then do
    when (carry' || (a' > 0x99)) $ do
      cpu.a .= 0x60
      cpu.carry .= True
    a'' <- use $ cpu.a
    when (half' || ((a'' .&. 0xf) > 0x9)) $ do
      cpu.a .= 0x6
  else do
    when carry' $ do
      cpu.a .= 0x60
    when half' $ do
      cpu.a .= 0x6

  a'' <- use $ cpu.a

  cpu %= (zero .~ isZero a'')
    . (half .~ False)
    . (cycleM %~ (+1))

  logI "DAA" (Rst $ showHex a'') Non Non

cpl :: GB ()
cpl = do
  cpu %= (a %~ complement)
    . (negative .~ True)
    . (half .~ True)
    . (cycleM %~ (+1))

  logI "CPL" A Non Non
      
ccf :: GB ()
ccf = do
  cpu %= (carry %~ not)
    . (negative .~ False)
    . (half .~ False)
    . (cycleM %~ (+1))

  logI "CCF" Carry Non Non

scf :: GB ()
scf = do
  cpu %= (carry .~ True)
    . (negative .~ False)
    . (half .~ False)
    . (cycleM %~ (+1))

  logI "SCF" Carry Non Non

halt :: GB ()
halt = do
  cpu %= (halting .~ True)
    . (cycleM %~ (+1))

  logI "HALT" Non Non Non

stop :: GB ()
stop = do
  cpu %= (stoping .~ True)
    . (cycleM %~ (+1))

  logI "STOP" Non Non Non

di :: GB ()
di = do
  cpu %= (ime .~ False)
    . (cycleM %~ (+1))

  logI "DI" Non Non Non

ei :: GB ()
ei = do
  cpu %= (ime .~ True)
    . (cycleM %~ (+1))

  logI "EI" Non Non Non

jp :: OP -> GB ()
jp op = do
  ww <- readOP16 WW
  zero' <- use $ cpu.zero
  carry' <- use $ cpu.carry
  pc' <- use $ cpu.pc
  case op of
    NotZero -> when (not zero') $ cpu.pc .= ww
    Zero -> when zero' $ cpu.pc .= ww
    NotCarry -> when (not carry') $ cpu.pc .= ww
    Carry -> when carry' $ cpu.pc .= ww
    Always -> cpu.pc .= ww
    P_HL -> cpu.pc <~ (use $ cpu.hl)

  pc'' <- use $ cpu.pc
  if pc' == pc'' then
    logI "JP" op (showHex ww) $ Info "No Jump"
  else
    logI "JP" op (showHex pc'') Non
    
jr :: OP -> GB ()
jr op = do
  i <- readPC
  pc' <- use $ cpu.pc
  zero' <- use $ cpu.zero
  carry' <- use $ cpu.carry
  let (pc'', _, _) = add_Word16_SignedWord8_IsCarryHalf pc' i
  case op of
    NotZero -> when (not zero') $ cpu.pc .= pc''
    Zero -> when zero' $ cpu.pc .= pc''
    NotCarry -> when (not carry') $ cpu.pc .= pc''
    Carry -> when carry' $ cpu.pc .= pc''
    Always -> cpu.pc .= pc''

  cpu.cycleM += 2
  pc''' <- use $ cpu.pc
  if pc' == pc''' then
    logI "JR" op (showSignedWord8 i) $ Info "No Jump"
  else
    logI "JR" op (showSignedWord8 i) (showHex pc''')

call :: OP -> GB ()
call op = do
  ww <- readOP16 WW
  pc' <- use $ cpu.pc
  zero' <- use $ cpu.zero
  carry' <- use $ cpu.carry
  let 
    (h',l') = sepWW $ pc'
    pp = push' h' >> push' l'
  case op of
    NotZero -> when (not zero') $ do cpu.pc .= ww >> pp
    Zero -> when zero' $ cpu.pc .= ww
    NotCarry -> when (not carry') $ do cpu.pc .= ww >> pp
    Carry -> when carry' $ do cpu.pc .= ww >> pp
    Always -> cpu.pc .= ww >> pp

  cpu.cycleM += 1
  pc'' <- use $ cpu.pc
  if pc' == pc'' then
    logI "CALL" op (showHex ww) $ Info "No Call"
  else
    logI "CALL" op (showHex ww) Non

rst :: Word16 -> GB ()
rst ww = do
  pc' <- use $ cpu.pc
  let (h',l') = sepWW pc'
  push' h'
  push' l'

  cpu %= (pc .~ ww)
    . (cycleM %~ (+8))

  logI "RST" (showHex ww) Non Non
 
reti :: GB ()
reti = do
  l' <- pop'
  h' <- pop'
  let pc' = toWW h' l'

  cpu %= (pc .~ pc')
    . (ime .~ True)
    . (cycleM %~ (+2))

  logI "RETI" (showHex pc') Non Non

ret :: OP -> GB ()
ret op = do
  let 
    pp = do
      l' <- pop'
      h' <- pop'
      pure $ toWW h' l'
  pc' <- use $ cpu.pc
  zero' <- use $ cpu.zero
  carry' <- use $ cpu.carry
  case op of 
    NotZero -> when (not zero') $ cpu.pc <~ pp
    Zero -> when zero' $ cpu.pc <~ pp
    NotCarry -> when (not carry') $ cpu.pc <~ pp
    Carry -> when carry' $ cpu.pc <~ pp
    Always -> cpu.pc <~ pp

  cpu.cycleM += 2
  pc'' <- use $ cpu.pc
  if pc' == pc'' then
    logI "RET" op (showHex pc') $ Info "No Return"
  else
    logI "RET" op (showHex pc'') Non


swap :: OP -> GB () 
swap op = do
  w <- readOP8 op
  let w' =  shift (w .&. 0xf) 4 .|. shift w (-4)
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ False)
    . (cycleM %~ (+1))

  logI "SWAP" op (Rst $ showHex w') Non

rlc :: OP -> GB ()
rlc op = do
  w <- readOP8 op
  let w' = rotateL w 1
  writeOP8 op w' 

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ (1 == shift w (-7)))
    . (cycleM %~ (+1))

  logI "RLC" op (Rst $ showHex w') Non

rl :: OP -> GB ()
rl op = do
  w <- readOP8 op
  carry' <- use $ cpu.carry
  let w' = shift w 1 .|. toNum carry'
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (carry .~ (1 == shift w (-7)))
    . (negative .~ False)
    . (half .~ False)
    . (cycleM %~ (+1))

  logI "RL" op (Rst $ showHex w') Non

rrc :: OP -> GB ()
rrc op = do
  w <- readOP8 op
  let w' = rotateR w 1
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ (1 == (w .&. 1)))
    . (cycleM %~ (+1))

  logI "RRC" op (Rst $ showHex w') Non

rr :: OP -> GB ()
rr op = do
  w <- readOP8 op
  carry' <- use $ cpu.carry
  let w' = shift (toNum carry') 7 .|. shift w (-1)
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ (1 == (w .&. 1)))
    . (cycleM %~ (+1))

  logI "RR" op (Rst $ showHex w') Non

sla :: OP -> GB ()
sla op = do
  w <- readOP8 op
  let w' = shift w 1
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ (1 == shift w (-7)))
    . (cycleM %~ (+2))

  logI "SLA" op (Rst $ showHex w') Non

sra :: OP -> GB ()
sra op = do
  w <- readOP8 op
  let w' = (w .&. 0x80) .|. shift w (-1)
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ (1 == (w .&. 1)))
    . (cycleM %~ (+1))

  logI "SRA" op (Rst $ showHex w') Non

srl :: OP -> GB ()
srl op = do
  w <- readOP8 op
  let w' = shift w (-1)
  writeOP8 op w'

  cpu %= (zero .~ isZero w')
    . (negative .~ False)
    . (half .~ False)
    . (carry .~ (1 == (w .&. 1)))
    . (cycleM %~ (+1))

  logI "SRL" op (Rst $ showHex w') Non

bit :: Int -> OP -> GB ()
bit i op = do
  w <- readOP8 op
  let w' = testBit w i
  writeOP8 op $ toNum w'

  cpu %= (zero .~ w')
    . (negative .~ False)
    . (half .~ True)
    . (cycleM %~ (+1))

  logI "BIT" i op (Rst $ show w')

set :: Int -> OP -> GB ()
set i op = do
  w <- readOP8 op
  let w' = setBit w i
  writeOP8 op w'

  cpu.cycleM += 1
  logI "SET" i op (Rst $ showHex w')

res :: Int -> OP -> GB ()
res i op = do
  w <- readOP8 op
  let w' = clearBit w i
  writeOP8 op w'

  cpu.cycleM += 1
  logI "RES" i op (Rst $ showHex w')




dispatch :: GB ()
dispatch = do
  instruction <- readPC
  case instruction of
    0x3e -> ld8 A W
    0x06 -> ld8 B W
    0x0e -> ld8 C W
    0x16 -> ld8 D W
    0x1e -> ld8 E W
    0x26 -> ld8 H W
    0x2e -> ld8 L W
    0x7f -> ld8 A A
    0x78 -> ld8 A B
    0x79 -> ld8 A C
    0x7a -> ld8 A D
    0x7b -> ld8 A E
    0x7c -> ld8 A H
    0x7d -> ld8 A L
    0x7e -> ld8 A P_HL
    0x0a -> ld8 A P_BC
    0x1a -> ld8 A P_DE
    0x47 -> ld8 B A
    0x40 -> ld8 B B
    0x41 -> ld8 B C
    0x42 -> ld8 B D
    0x43 -> ld8 B E
    0x44 -> ld8 B H
    0x45 -> ld8 B L
    0x46 -> ld8 B P_HL
    0x4f -> ld8 C A
    0x48 -> ld8 C B
    0x49 -> ld8 C C
    0x4a -> ld8 C D
    0x4b -> ld8 C E
    0x4c -> ld8 C H
    0x4d -> ld8 C L
    0x4e -> ld8 C P_HL
    0x57 -> ld8 D A
    0x50 -> ld8 D B
    0x51 -> ld8 D C
    0x52 -> ld8 D D
    0x53 -> ld8 D E
    0x54 -> ld8 D H
    0x55 -> ld8 D L
    0x56 -> ld8 D P_HL
    0x5f -> ld8 E A
    0x58 -> ld8 E B
    0x59 -> ld8 E C
    0x5a -> ld8 E D
    0x5b -> ld8 E E
    0x5c -> ld8 E H
    0x5d -> ld8 E L
    0x5e -> ld8 E P_HL
    0x67 -> ld8 H A
    0x60 -> ld8 H B
    0x61 -> ld8 H C
    0x62 -> ld8 H D
    0x63 -> ld8 H E
    0x64 -> ld8 H H
    0x65 -> ld8 H L
    0x66 -> ld8 H P_HL
    0x6f -> ld8 L A
    0x68 -> ld8 L B
    0x69 -> ld8 L C
    0x6a -> ld8 L D
    0x6b -> ld8 L E
    0x6c -> ld8 L H
    0x6d -> ld8 L L
    0x6e -> ld8 L P_HL

    0x70 -> ld8 P_HL B
    0x71 -> ld8 P_HL C
    0x72 -> ld8 P_HL D
    0x73 -> ld8 P_HL E
    0x74 -> ld8 P_HL H
    0x75 -> ld8 P_HL L
    0x36 -> ld8 P_HL W
    0x02 -> ld8 P_BC A
    0x12 -> ld8 P_DE A
    0x77 -> ld8 P_HL A
    0xea -> ld8 P_WW A

    0xf0 -> ld8 A P_FF00_W
    0xf2 -> ld8 A P_FF00_C 
    0xfa -> ld8 A P_WW
    0xe0 -> ld8 P_FF00_W A
    0xe2 -> ld8 P_FF00_C A

    0x22 -> ld8_id_p_hl_a succ "LDI"
    0x2a -> ld8_id_a_p_hl succ "LDI"
    0x32 -> ld8_id_p_hl_a pred "LDD"
    0x3a -> ld8_id_a_p_hl pred "LDD"

    0x01 -> ld16 BC WW
    0x11 -> ld16 DE WW
    0x21 -> ld16 HL WW
    0x31 -> ld16 SP WW
    0xf9 -> ld16 SP HL
    0x08 -> ld16 P_WW SP
    0xf8 -> ld16_hl_sp_w

    0xf5 -> push AF
    0xc5 -> push BC
    0xd5 -> push DE
    0xe5 -> push HL
    0xf1 -> pop AF
    0xc1 -> pop BC
    0xd1 -> pop DE
    0xe1 -> pop HL

    0x87 -> add A
    0x80 -> add B
    0x81 -> add C
    0x82 -> add D
    0x83 -> add E
    0x84 -> add H
    0x85 -> add L
    0x86 -> add P_HL 
    0xc6 -> add W

    0x8f -> adc A
    0x88 -> adc B
    0x89 -> adc C
    0x8a -> adc D
    0x8b -> adc E
    0x8c -> adc H
    0x8d -> adc L
    0x8e -> adc P_HL 
    0xce -> adc W

    0x97 -> sub A
    0x90 -> sub B
    0x91 -> sub C
    0x92 -> sub D
    0x93 -> sub E
    0x94 -> sub H
    0x95 -> sub L
    0x96 -> sub P_HL 
    0xd6 -> sub W

    0x9f -> sbc A
    0x98 -> sbc B
    0x99 -> sbc C
    0x9a -> sbc D
    0x9b -> sbc E
    0x9c -> sbc H
    0x9d -> sbc L
    0x9e -> sbc P_HL 
    0xde -> sbc W

    0xa7 -> and A
    0xa0 -> and B
    0xa1 -> and C
    0xa2 -> and D
    0xa3 -> and E
    0xa4 -> and H
    0xa5 -> and L
    0xa6 -> and P_HL 
    0xe6 -> and W

    0xb7 -> or A
    0xb0 -> or B
    0xb1 -> or C
    0xb2 -> or D
    0xb3 -> or E
    0xb4 -> or H
    0xb5 -> or L
    0xb6 -> or P_HL 
    0xf6 -> or W

    0xaf -> xor A
    0xa8 -> xor B
    0xa9 -> xor C
    0xaa -> xor D
    0xab -> xor E
    0xac -> xor H
    0xad -> xor L
    0xae -> xor P_HL 
    0xee -> xor W

    0xbf -> cp A
    0xb8 -> cp B
    0xb9 -> cp C
    0xba -> cp D
    0xbb -> cp E
    0xbc -> cp H
    0xbd -> cp L
    0xbe -> cp P_HL 
    0xfe -> cp W

    0x3c -> inc8 A
    0x04 -> inc8 B
    0x0c -> inc8 C
    0x14 -> inc8 D
    0x1c -> inc8 E
    0x24 -> inc8 H
    0x2c -> inc8 L
    0x34 -> inc8 P_HL 

    0x3d -> dec8 A
    0x05 -> dec8 B
    0x0d -> dec8 C
    0x15 -> dec8 D
    0x1d -> dec8 E
    0x25 -> dec8 H
    0x2d -> dec8 L
    0x35 -> dec8 P_HL 

    0x09 -> add_hl BC
    0x19 -> add_hl DE
    0x29 -> add_hl HL
    0x39 -> add_hl SP
    0xe8 -> add_sp

    0x03 -> inc16 BC
    0x13 -> inc16 DE
    0x23 -> inc16 HL
    0x33 -> inc16 SP

    0x0b -> dec16 BC
    0x1b -> dec16 DE
    0x2b -> dec16 HL
    0x3b -> dec16 SP

    0x07 -> rlc A
    0x17 -> rl A
    0x0f -> rrc A 
    0x1f -> rr A 

    0x27 -> daa 

    0x2f -> cpl 
    0x3f -> ccf 
    0x37 -> scf 

    0xc3 -> jp Always
    0xc2 -> jp NotZero
    0xca -> jp Zero
    0xd2 -> jp NotCarry
    0xda -> jp Carry
    0xe9 -> jp P_HL
    0x18 -> jr Always   
    0x20 -> jr NotZero  
    0x28 -> jr Zero     
    0x30 -> jr NotCarry 
    0x38 -> jr Carry    
    0xcd -> call Always     
    0xc4 -> call NotZero    
    0xcc -> call Zero       
    0xd4 -> call NotCarry   
    0xdc -> call Carry      
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
    0xc8 -> ret Zero     
    0xd0 -> ret NotCarry 
    0xd8 -> ret Carry    
    0xd9 -> reti

    0xf3 -> di 
    0xfb -> ei 

    0x76 -> halt 

    0x10 -> do
      instruction' <- readPC
      cpu.cycleM += 1
      case instruction' of 
        0x00 -> stop
        _ -> error $ "CPU undefind instruction 0x10 " ++ showHex instruction'

    0x00 -> nop 

    0xcb -> do
      instruction' <- readPC
      cpu.cycleM += 1
      case instruction' of 
        0x37 -> swap A
        0x30 -> swap B
        0x31 -> swap C
        0x32 -> swap D
        0x33 -> swap E
        0x34 -> swap H
        0x35 -> swap L
        0x36 -> swap P_HL 

        0x07 -> rlc A
        0x00 -> rlc B
        0x01 -> rlc C
        0x02 -> rlc D
        0x03 -> rlc E
        0x04 -> rlc H
        0x05 -> rlc L
        0x06 -> rlc P_HL 

        0x17 -> rl A
        0x10 -> rl B
        0x11 -> rl C
        0x12 -> rl D
        0x13 -> rl E
        0x14 -> rl H
        0x15 -> rl L
        0x16 -> rl P_HL 

        0x0f -> rrc A
        0x08 -> rrc B
        0x09 -> rrc C
        0x0a -> rrc D
        0x0b -> rrc E
        0x0c -> rrc H
        0x0d -> rrc L
        0x0e -> rrc P_HL 

        0x1f -> rr A
        0x18 -> rr B
        0x19 -> rr C
        0x1a -> rr D
        0x1b -> rr E
        0x1c -> rr H
        0x1d -> rr L
        0x1e -> rr P_HL

        0x27 -> sla A
        0x20 -> sla B
        0x21 -> sla C
        0x22 -> sla D
        0x23 -> sla E
        0x24 -> sla H
        0x25 -> sla L
        0x26 -> sla P_HL

        0x2f -> sra A
        0x28 -> sra B
        0x29 -> sra C
        0x2a -> sra D
        0x2b -> sra E
        0x2c -> sra H
        0x2d -> sra L
        0x2e -> sra P_HL 

        0x3f -> srl A
        0x38 -> srl B
        0x39 -> srl C
        0x3a -> srl D
        0x3b -> srl E
        0x3c -> srl H
        0x3d -> srl L
        0x3e -> srl P_HL 

        0x47 -> bit 0 A
        0x40 -> bit 0 B
        0x41 -> bit 0 C
        0x42 -> bit 0 D
        0x43 -> bit 0 E
        0x44 -> bit 0 H
        0x45 -> bit 0 L
        0x46 -> bit 0 P_HL
        0x4f -> bit 1 A
        0x48 -> bit 1 B
        0x49 -> bit 1 C
        0x4a -> bit 1 D
        0x4b -> bit 1 E
        0x4c -> bit 1 H
        0x4d -> bit 1 L
        0x4e -> bit 1 P_HL
        0x57 -> bit 2 A
        0x50 -> bit 2 B
        0x51 -> bit 2 C
        0x52 -> bit 2 D
        0x53 -> bit 2 E
        0x54 -> bit 2 H
        0x55 -> bit 2 L
        0x56 -> bit 2 P_HL
        0x5f -> bit 3 A
        0x58 -> bit 3 B
        0x59 -> bit 3 C
        0x5a -> bit 3 D
        0x5b -> bit 3 E
        0x5c -> bit 3 H
        0x5d -> bit 3 L
        0x5e -> bit 3 P_HL
        0x67 -> bit 4 A
        0x60 -> bit 4 B
        0x61 -> bit 4 C
        0x62 -> bit 4 D
        0x63 -> bit 4 E
        0x64 -> bit 4 H
        0x65 -> bit 4 L
        0x66 -> bit 4 P_HL
        0x6f -> bit 5 A
        0x68 -> bit 5 B
        0x69 -> bit 5 C
        0x6a -> bit 5 D
        0x6b -> bit 5 E
        0x6c -> bit 5 H
        0x6d -> bit 5 L
        0x6e -> bit 5 P_HL
        0x77 -> bit 6 A
        0x70 -> bit 6 B
        0x71 -> bit 6 C
        0x72 -> bit 6 D
        0x73 -> bit 6 E
        0x74 -> bit 6 H
        0x75 -> bit 6 L
        0x76 -> bit 6 P_HL
        0x7f -> bit 7 A
        0x78 -> bit 7 B
        0x79 -> bit 7 C
        0x7a -> bit 7 D
        0x7b -> bit 7 E
        0x7c -> bit 7 H
        0x7d -> bit 7 L
        0x7e -> bit 7 P_HL

        0xc7 -> set 0 A
        0xc0 -> set 0 B
        0xc1 -> set 0 C
        0xc2 -> set 0 D
        0xc3 -> set 0 E
        0xc4 -> set 0 H
        0xc5 -> set 0 L
        0xc6 -> set 0 P_HL
        0xcf -> set 1 A
        0xc8 -> set 1 B
        0xc9 -> set 1 C
        0xca -> set 1 D
        0xcb -> set 1 E
        0xcc -> set 1 H
        0xcd -> set 1 L
        0xce -> set 1 P_HL
        0xd7 -> set 2 A
        0xd0 -> set 2 B
        0xd1 -> set 2 C
        0xd2 -> set 2 D
        0xd3 -> set 2 E
        0xd4 -> set 2 H
        0xd5 -> set 2 L
        0xd6 -> set 2 P_HL
        0xdf -> set 3 A
        0xd8 -> set 3 B
        0xd9 -> set 3 C
        0xda -> set 3 D
        0xdb -> set 3 E
        0xdc -> set 3 H
        0xdd -> set 3 L
        0xde -> set 3 P_HL
        0xe7 -> set 4 A
        0xe0 -> set 4 B
        0xe1 -> set 4 C
        0xe2 -> set 4 D
        0xe3 -> set 4 E
        0xe4 -> set 4 H
        0xe5 -> set 4 L
        0xe6 -> set 4 P_HL
        0xef -> set 5 A
        0xe8 -> set 5 B
        0xe9 -> set 5 C
        0xea -> set 5 D
        0xeb -> set 5 E
        0xec -> set 5 H
        0xed -> set 5 L
        0xee -> set 5 P_HL
        0xf7 -> set 6 A
        0xf0 -> set 6 B
        0xf1 -> set 6 C
        0xf2 -> set 6 D
        0xf3 -> set 6 E
        0xf4 -> set 6 H
        0xf5 -> set 6 L
        0xf6 -> set 6 P_HL
        0xff -> set 7 A
        0xf8 -> set 7 B
        0xf9 -> set 7 C
        0xfa -> set 7 D
        0xfb -> set 7 E
        0xfc -> set 7 H
        0xfd -> set 7 L
        0xfe -> set 7 P_HL

        0x87 -> res 0 A
        0x80 -> res 0 B
        0x81 -> res 0 C
        0x82 -> res 0 D
        0x83 -> res 0 E
        0x84 -> res 0 H
        0x85 -> res 0 L
        0x86 -> res 0 P_HL
        0x8f -> res 1 A
        0x88 -> res 1 B
        0x89 -> res 1 C
        0x8a -> res 1 D
        0x8b -> res 1 E
        0x8c -> res 1 H
        0x8d -> res 1 L
        0x8e -> res 1 P_HL
        0x97 -> res 2 A
        0x90 -> res 2 B
        0x91 -> res 2 C
        0x92 -> res 2 D
        0x93 -> res 2 E
        0x94 -> res 2 H
        0x95 -> res 2 L
        0x96 -> res 2 P_HL
        0x9f -> res 3 A
        0x98 -> res 3 B
        0x99 -> res 3 C
        0x9a -> res 3 D
        0x9b -> res 3 E
        0x9c -> res 3 H
        0x9d -> res 3 L
        0x9e -> res 3 P_HL
        0xa7 -> res 4 A
        0xa0 -> res 4 B
        0xa1 -> res 4 C
        0xa2 -> res 4 D
        0xa3 -> res 4 E
        0xa4 -> res 4 H
        0xa5 -> res 4 L
        0xa6 -> res 4 P_HL
        0xaf -> res 5 A
        0xa8 -> res 5 B
        0xa9 -> res 5 C
        0xaa -> res 5 D
        0xab -> res 5 E
        0xac -> res 5 H
        0xad -> res 5 L
        0xae -> res 5 P_HL
        0xb7 -> res 6 A
        0xb0 -> res 6 B
        0xb1 -> res 6 C
        0xb2 -> res 6 D
        0xb3 -> res 6 E
        0xb4 -> res 6 H
        0xb5 -> res 6 L
        0xb6 -> res 6 P_HL
        0xbf -> res 7 A
        0xb8 -> res 7 B
        0xb9 -> res 7 C
        0xba -> res 7 D
        0xbb -> res 7 E
        0xbc -> res 7 H
        0xbd -> res 7 L
        0xbe -> res 7 P_HL

        _ -> error $ showHex instruction'
    _ -> error $ showHex instruction
