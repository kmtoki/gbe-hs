module Gameboy.CPU where

import Prelude hiding (read, cycle, log, or, and)

import Gameboy.MBC
import Gameboy.Logger
import Gameboy.Utils hiding (set,bit,xor)

import qualified Numeric as N
import qualified Data.Bits as B
import qualified Data.Vector as V

type CPU a = StateT CPUState (StateT MBCState (StateT LoggerState IO)) a

--newtype CPU a = CPU {
--    runCPU :: StateT CPUState (MBC Logger) a
--  }
--  deriving (Functor, Applicative, Monad, MonadState CPUState, MonadTrans, MonadIO)

data CPUState = CPUState {
    _a, __f, _b, _c, _d, _e, _h, _l :: Word8,
    _sp, _pc :: Word16,
    __zero, __negative, __half, __carry :: Bool,
    _serial_counter :: Word8,
    _serial_buffer :: V.Vector Word8,
    _ime :: Bool,
    --_ime_prev :: Bool,
    _halting :: Bool,
    _stoping :: Bool,
    _cycle :: Int
  } deriving Show

makeLenses ''CPUState

data OP
  = A | F | B | C | D | E | H | L
  | AF | BC | DE | HL | SP | PC
  | P_BC | P_DE | P_HL | P_WW
  | P_FF00_C | P_FF00_W
  | W | WW
  | Zero | NotZero | Carry | NotCarry | Always
  deriving Show


newCPUState :: CPUState
newCPUState = CPUState {
  _a = 0,
  __f = 0xb0,
  _b = 0x13,
  _c = 0,
  _d = 0xd8,
  _e = 0,
  _h = 0x01,
  _l = 0x4d,
  _sp = 0xfffe,
  _pc = 0x100,
  __zero = True,
  __negative = False,
  __half = True,
  __carry = True,
  _serial_counter = 0,
  _serial_buffer = V.empty,
  _ime = False,
  --_ime_prev = False,
  _halting = False,
  _stoping = False,
  _cycle = 0
  }

f :: Lens' CPUState Word8
f = lens get set
  where
    bool = (1 ==)
    get = __f
    set c w = c {
        __f = w .&. shift 0xf 4,
        __carry = bool (shift w (-4) .&. 1),
        __half = bool (shift w (-5) .&. 1),
        __negative = bool (shift w (-6) .&. 1),
        __zero = bool (shift w (-7) .&. 1)
      } -- ignore/reset lower 4 bits always

carry :: Lens' CPUState Bool
carry = lens get set
  where
    get = __carry
    set c b =
      if b then
        c { __carry = b, __f = setBit (__f c) 4 }
      else
        c { __carry = b, __f = clearBit (__f c) 4 }

half :: Lens' CPUState Bool
half = lens get set
  where
    get = __half
    set c b =
      if b then
        c { __half = b, __f = setBit (__f c) 5 }
      else
        c { __half = b, __f = clearBit (__f c) 5 }

negative :: Lens' CPUState Bool
negative = lens get set
  where
    get = __negative
    set c b =
      if b then
        c { __negative = b, __f = setBit (__f c) 6 }
      else
        c { __negative = b, __f = clearBit (__f c) 6 }

zero :: Lens' CPUState Bool
zero = lens get set
  where
    get = __zero
    set c b =
      if b then
        c { __zero = b, __f = setBit (__f c) 7 }
      else
        c { __zero = b, __f = clearBit (__f c) 7 }


af :: Lens' CPUState Word16
af = lens get set
  where
    get c = toWW (_a c) (__f c)
    set c ww = c { _a = _a', __f = __f' }
      where
        (_a',__f'') = sepWW ww
        __f' = __f'' .&. 0xf0

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



read :: Address i => i -> CPU Word8
read i = lift $ do
  r <- use reader
  a <- r $ toInt i
  --lift $ logger $ ("read " ++ showHex (toInt i) ++ " -> " ++ showHex a) 
  pure a

write :: Address i => i -> Word8 -> CPU ()
write i w = lift $ do
  wr <- use writer
  wr (toInt i) w

log :: String -> CPU ()
log = lift . lift . logger

push' :: Word8 -> CPU ()
push' i = do
  sp' <- use sp 
  write sp' i
  sp -= 1

pop' :: CPU Word8
pop' = do
  sp' <- use sp
  w <- read sp'
  sp += 1
  pure w

showCPUState :: CPUState -> String
showCPUState cpu =
  ""
  --"CPU"
    ++ " pc:" ++ showHex (cpu^.pc)
    ++ " sp:" ++ showHex (cpu^.sp)
    ++ " a:" ++ showHex (cpu^.a)
    ++ " f:" ++ showHex (cpu^.f)
    ++ " b:" ++ showHex (cpu^.d)
    ++ " c:" ++ showHex (cpu^.c)
    ++ " d:" ++ showHex (cpu^.d)
    ++ " e:" ++ showHex (cpu^.e)
    ++ " h:" ++ showHex (cpu^.h)
    ++ " l:" ++ showHex (cpu^.l)
    ++ " Z:" ++ show (cpu^.zero&toNum)
    ++ " N:" ++ show (cpu^.negative&toNum)
    ++ " H:" ++ show (cpu^.half&toNum)
    ++ " C:" ++ show (cpu^.carry&toNum)
    ++ " ime:" ++ show (cpu^.ime&toNum)
    ++ " sb:" ++ (cpu^.serial_buffer & V.map (chr.fi) & V.toList & show)
  where
    showHex x = N.showHex x ""


executeCPU :: CPU ()
executeCPU = do
  cycle .= 0

  cpu' <- get
  instr <- getInstructionName
  when (instr /= "nop") $ do
    liftIO $ putStrLn instr
    liftIO $ putStrLn (showCPUState cpu')

  dispatch
  serial
  --timer
  interrupt

--timer :: CPU ()
--timer = do

serial :: CPU ()
serial = do
  sc <- read SC
  let 
    sck = testBit sc 0
    select = testBit sc 1
    start = testBit sc 7
  when start $ do
    scc <- use serial_counter
    serial_counter += 1
    when (scc > 7) $ do
      sb <- read SB
      if sb == 0xff then do
        sbs <- use serial_buffer
        serial_buffer .= V.empty
        log ("Serial: " ++ (V.toList $ V.map (chr.fi) sbs))
      else do
        serial_buffer %= (flip V.snoc sb)

      write SC $ clearBit sc 7
      interrupt

interrupt :: CPU ()
interrupt = do
  master <- use ime
  --ime_prev .= master
  when master $ do
    enable <- read IE
    request <- read IF
    let 
      (addr, bit) = 
        -- VBlank
        if testBit enable 0 && testBit request 0 then
          (0x40, 0)
        -- LCDC STAT
        else if testBit enable 1 && testBit request 1 then
          (0x48, 1)
        -- Time Overflow
        else if testBit enable 2 && testBit request 2 then
          (0x50, 2)
        -- Serial transfter completion
        else if testBit enable 3 && testBit request 3 then
          (0x58, 3)
        -- Keypad
        else if testBit enable 4 && testBit request 4 then
          (0x60, 4)
        -- Nothing
        else
          (0, 0)
    when (not (addr == 0 && bit == 0)) $ do
      --nop
      --nop
      ime .= False
      write IE $ clearBit enable bit
      write IF $ clearBit request bit
      pc' <- use pc
      let (h',l') = sepWW pc'
      push' l'
      push' h'
      cycle += 5
      pc .= addr




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
add16_IsCarryHalf w1 w2 = (w3, w1 < w3, w4 > 0x7fff)
  where
    w3 = w1 - w2 
    w4 = (w1 .&. 0x7fff) + (w2 .&. 0x7fff)

sub16_IsCarryHalf :: Word16 -> Word16 -> (Word16, Bool, Bool)
sub16_IsCarryHalf w1 w2 = (w3, w1 < w3, w1 < w4)
  where
    w3 = w1 - w2
    w4 = (w1 .&. 0x7fff) - (w2 .&. 0x7fff)

add_Word16_SingedWord8_IsCarryHalf :: Word16 -> Word8 -> (Word16, Bool, Bool)
add_Word16_SingedWord8_IsCarryHalf w i =
  let i' = fi $ clearBit i 7 in
  if testBit i 7 then
    let 
      w' = w - i'
      h' = (w' .&. 0xf) - (i' .&. 0xf)
    in (w', w < w', w < h')
  else
    let 
      w' = w + i'
      h' = (w' .&. 0xf) + (i' .&. 0xf)
    in (w', w > w', h' > 0xf)
  

readPC :: CPU Word8
readPC = do
  pc' <- use pc
  w <- read pc'
  pc += 1
  pure w

readOP8 :: OP -> CPU Word8
readOP8 op = do
  case op of
    A -> use a
    F -> use f
    B -> use b
    C -> use c
    D -> use d
    E -> use e
    H -> use h
    L -> use l
    W ->  cycle += 1 >> readPC
    P_BC -> cycle += 1 >> use bc >>= read
    P_DE -> cycle += 1 >> use de >>= read
    P_HL -> cycle += 1 >> use hl >>= read
    P_WW -> cycle += 3 >> (flip toWW <$> readPC <*> readPC) >>= read . toInt
    P_FF00_C -> cycle += 1 >> use c >>= read . (0xff00 +) . toInt
    P_FF00_W -> cycle += 2 >> readPC >>= read . (0xff00 +) . toInt

writeOP8 :: OP -> Word8 -> CPU ()
writeOP8 op w = do
  case op of
    A -> a .= w
    F -> f .= w
    B -> b .= w
    C -> c .= w
    D -> d .= w
    E -> e .= w
    H -> h .= w
    L -> l .= w
    P_BC -> cycle += 1 >> use bc >>= flip write w
    P_DE -> cycle += 1 >> use de >>= flip write w
    P_HL -> cycle += 1 >> use hl >>= flip write w
    P_WW -> cycle += 3 >> (flip toWW <$> readPC <*> readPC) >>= flip write w
    P_FF00_C -> cycle += 1 >> use c >>= flip write w . (0xff00 +) . toInt
    P_FF00_W -> cycle += 2 >> readPC >>= flip write w . (0xff00 +) . toInt
    _ -> error $ show op

readOP16 :: OP -> CPU Word16
readOP16 op = do
  case op of
    AF -> use af
    BC -> use bc
    DE -> use de
    HL -> use hl
    SP -> use sp
    PC -> use pc
    WW -> cycle += 2 >> (flip toWW <$> readPC <*> readPC)

writeOP16 :: OP -> Word16 -> CPU ()
writeOP16 op ww = do
  case op of
    AF -> af .= ww
    BC -> bc .= ww
    DE -> de .= ww
    HL -> hl .= ww
    SP -> sp .= ww
    PC -> pc .= ww
    P_WW -> do 
      cycle += 3
      addr <- flip toWW <$> readPC <*> readPC
      let (h, l) = sepWW ww
      write addr l
      write (addr + 1) h

modifyOP8 :: OP -> (Word8 -> Word8) -> CPU ()
modifyOP8 op f = do
  w <- readOP8 op
  writeOP8 op $ f w

modifyOP16 :: OP -> (Word16 -> Word16) -> CPU ()
modifyOP16 op f = do
  w <- readOP16 op
  writeOP16 op $ f w


nop' :: CPU ()
nop' = do
  cycle += 1


nop :: CPU ()
nop = do
  cycle += 1

ld8 :: OP -> OP -> CPU ()
ld8 op1 op2 = do
  w <- readOP8 op2
  writeOP8 op1 w

  cycle += 1 

ld16 :: OP -> OP -> CPU ()
ld16 op1 op2 = do
  ww <- readOP16 op2
  writeOP16 op1 ww

  cycle += 2 

ld8_id_a_p_hl :: (Word16 -> Word16) -> CPU ()
ld8_id_a_p_hl f = do
  hl' <- use hl
  w <- read hl'
  a .= w
  hl .= f hl'

  cycle += 2

ld8_id_p_hl_a :: (Word16 -> Word16) -> CPU ()
ld8_id_p_hl_a f = do
  hl' <- use hl
  a' <- use a
  write hl' a'
  hl .= f hl'

  cycle += 2

ld16_hl_sp_w :: CPU ()
ld16_hl_sp_w = do
  w <- readPC
  sp' <- use sp 
  let (sp'',c',h') = add_Word16_SingedWord8_IsCarryHalf sp' w
  hl .= sp''

  zero .= False
  negative .= False
  half .= h'
  carry .= c'
  cycle += 3

push :: OP -> CPU ()
push op = do
  ww <- readOP16 op
  let (h',l') = sepWW ww
  push' l'
  push' h'

  cycle += 4

pop :: OP -> CPU ()
pop op = do
  h' <- pop'
  l' <- pop'
  writeOP16 op $ toWW h' l'

  cycle += 3

add :: OP -> CPU ()
add op = do
  a' <- use a
  w <- readOP8 op
  let (a'', c', h') = add8_IsCarryHalf a' w
  a .= a''

  zero .= isZero a''
  negative .= False
  half .= h'
  carry .= c'
  cycle += 1

adc :: OP -> CPU ()
adc op = do
  a' <- use a
  w <- readOP8 op
  c' <- use carry
  let 
    (a'', c'', h'') = add8_IsCarryHalf a' w
    (a''', c''', h''') = add8_IsCarryHalf a'' $ toNum c'
  a .= a'''

  zero .= isZero a'''
  negative .= False
  half .= (h'' || h''')
  carry .= (c'' || c''')
  cycle += 1

sub :: OP -> CPU ()
sub op = do
  a' <- use a
  w <- readOP8 op
  let (a'', c', h') = sub8_IsCarryHalf a' w
  a .= a''

  zero .= isZero a''
  negative .= True
  half .= h'
  carry .= c'
  cycle += 1

sbc :: OP -> CPU ()
sbc op = do
  a' <- use a
  w <- readOP8 op
  c' <- use carry
  let 
    (a'', c'', h'') = sub8_IsCarryHalf a' w
    (a''', c''', h''') = sub8_IsCarryHalf a'' $ toNum c'
  a .= a'''

  zero .= isZero a'''
  negative .= True
  half .= (h'' || h''')
  carry .= (c'' || c''')
  cycle += 1


and :: OP -> CPU ()
and op = do
  a' <- use a
  w <- readOP8 op
  let a'' = a' .&. w
  a .= a''

  zero .= isZero a''
  negative .= False
  half .= True
  carry .= False
  cycle += 1

or :: OP -> CPU ()
or op = do
  a' <- use a
  w <- readOP8 op
  let a'' = a' .|. w
  a .= a''

  zero .= isZero a''
  negative .= False
  half .= False
  carry .= False
  cycle += 1

xor :: OP -> CPU ()
xor op = do
  a' <- use a
  w <- readOP8 op
  let a'' = a' `B.xor` w
  a .= a''

  zero .= isZero a''
  negative .= False
  half .= False
  carry .= False
  cycle += 1


cp :: OP -> CPU ()
cp op = do
  a' <- use a
  w <- readOP8 op
  let (a'', c', h') = sub8_IsCarryHalf a' w

  zero .= isZero a''
  negative .= True
  half .= h'
  carry .= c'
  cycle += 1

inc8 :: OP -> CPU ()
inc8 op = do
  w <- readOP8 op
  let (w', c', h') = add8_IsCarryHalf w 1
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= h'
  cycle += 1

dec8 :: OP -> CPU ()
dec8 op = do
  w <- readOP8 op
  let (w', c', h') = sub8_IsCarryHalf w 1
  writeOP8 op w'

  zero .= isZero w'
  negative .= True
  half .= h'
  cycle += 1

add_hl :: OP -> CPU ()
add_hl op = do
  hl' <- use hl
  ww <- readOP16 op
  let (hl'',c',h') = add16_IsCarryHalf hl' ww
  hl .= hl'

  negative .= False
  half .= h'
  carry .= c'
  cycle += 2

add_sp :: CPU ()
add_sp = do
  sp' <- use sp
  w <- readPC
  let (sp'', c', h') = add_Word16_SingedWord8_IsCarryHalf sp' w
  sp .= sp''

  zero .= False
  negative .= False
  half .= h'
  carry .= c'
  cycle += 2

inc16 :: OP -> CPU ()
inc16 op = do
  ww <- readOP16 op
  writeOP16 op (ww + 1)
  
  cycle += 2

dec16 :: OP -> CPU ()
dec16 op = do
  ww <- readOP16 op
  writeOP16 op (ww - 1)
  
  cycle += 2

daa :: CPU ()
daa = do
  a' <- use a
  negative' <- use negative
  half' <- use half
  carry' <- use carry
  if not negative' then do
    when (carry' || (a' > 0x99)) $ do
      a += 0x60
      carry .= True
    a'' <- use a
    when (half' || ((a'' .&. 0xf) > 0x9)) $ do
      a += 0x6
  else do
    when carry' $ do
      a -= 0x60
    when half' $ do
      a -= 0x6

  a'' <- use a
  zero .= isZero a''
  half .= False
  cycle += 1

cpl :: CPU ()
cpl = do
  a %= complement

  negative .= True
  half .= True
  cycle += 1
      
ccf :: CPU ()
ccf = do
  carry %= not

  negative .= False
  half .= False
  cycle += 1

scf :: CPU ()
scf = do
  carry .= True

  negative .= False
  half .= False
  cycle += 1

halt :: CPU ()
halt = do
  halting .= True

  cycle += 1

stop :: CPU ()
stop = do
  --halting .= True
  stoping .= True

  cycle += 1

di :: CPU ()
di = do
  --write IF 0x0
  --write IE 0x0
  ime .= False

  cycle += 1

ei :: CPU ()
ei = do
  --write IF 0xff
  --write IE 0xff
  ime .= False

  cycle += 1

jp :: OP -> CPU ()
jp op = do
  ww <- readOP16 WW
  zero' <- use zero
  carry' <- use carry
  case op of
    NotZero -> when (not zero') $ pc .= ww
    Zero -> when zero' $ pc .= ww
    NotCarry -> when (not carry') $ pc .= ww
    Carry -> when carry' $ pc .= ww
    Always -> pc .= ww
    P_HL -> pc <~ use hl

jr :: OP -> CPU ()
jr op = do
  pc' <- use pc
  i <- readPC
  zero' <- use zero
  carry' <- use carry
  let (pc'', _, _) = add_Word16_SingedWord8_IsCarryHalf pc' i
  case op of
    NotZero -> when (not zero') $ pc .= pc''
    Zero -> when zero' $ pc .= pc''
    NotCarry -> when (not carry') $ pc .= pc''
    Carry -> when carry' $ pc .= pc''
    Always -> pc .= pc''

  cycle += 2

call :: OP -> CPU ()
call op = do
  pc' <- use pc
  ww <- readOP16 WW
  zero' <- use zero
  carry' <- use carry
  let (h',l') = sepWW $ pc' + 1
  push' l'
  push' h'
  case op of
    NotZero -> when (not zero') $ pc .= ww
    Zero -> when zero' $ pc .= ww
    NotCarry -> when (not carry') $ pc .= ww
    Carry -> when carry' $ pc .= ww
    Always -> pc .= ww

  cycle += 1

rst :: Word16 -> CPU ()
rst ww = do
  pc' <- use pc
  let (h',l') = sepWW pc'
  push' l'
  push' h'
  pc .= ww

  cycle .= 8
 
reti :: CPU ()
reti = do
  h' <- pop'
  l' <- pop'
  pc .= toWW h' l'
  ime .= True

  cycle += 2

ret :: OP -> CPU ()
ret op = do
  h' <- pop'
  l' <- pop'
  zero' <- use zero
  carry' <- use carry
  let pc' = toWW h' l'
  case op of
    NotZero -> when (not zero') $ pc .= pc'
    Zero -> when zero' $ pc .= pc'
    NotCarry -> when (not carry') $ pc .= pc'
    Carry -> when carry' $ pc .= pc'
    Always -> pc .= pc'

  cycle += 2



swap :: OP -> CPU () 
swap op = do
  w <- readOP8 op
  let w' =  shift (w .&. 0xf) 4 .|. shift w (-4)
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False
  carry .= False
  cycle += 1

rlc :: OP -> CPU ()
rlc op = do
  w <- readOP8 op
  carry .= (1 == shift w (-7))
  let w' = rotateL w 1
  writeOP8 op w' 

  zero .= isZero w'
  negative .= False
  half .= False

  cycle += 1

rl :: OP -> CPU ()
rl op = do
  w <- readOP8 op
  carry' <- use carry
  carry .= (1 == shift w (-7))
  let w' = shift w 1 .|. toNum carry'
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False
  cycle += 1

rrc :: OP -> CPU ()
rrc op = do
  w <- readOP8 op
  carry .= (1 == (w .&. 1))
  let w' = rotateR w 1
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False
  cycle += 1


rr :: OP -> CPU ()
rr op = do
  w <- readOP8 op
  carry' <- use carry
  carry .= (1 == (w .&. 1))
  let w' = shift (toNum carry') 7 .|. shift w (-1)
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False

  cycle += 1

sla :: OP -> CPU ()
sla op = do
  w <- readOP8 op
  carry .= (1 == shift w (-7))
  let w' = shift w 1
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False
  cycle += 2

sra :: OP -> CPU ()
sra op = do
  w <- readOP8 op
  carry .= (1 == (w .&. 1))
  let w' = (w .&. 0x80) .|. shift w (-1)
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False
  cycle += 1

srl :: OP -> CPU ()
srl op = do
  w <- readOP8 op
  carry .= (1 == (w .&. 1))
  let w' = shift w (-1)
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False
  cycle += 1

bit :: Int -> OP -> CPU ()
bit i op = do
  w <- readOP8 op
  let w' = testBit w i
  a .= toNum w'

  zero .= w'
  negative .= False
  half .= True
  cycle += 1

set :: Int -> OP -> CPU ()
set i op = do
  w <- readOP8 op
  let w' = setBit w i
  writeOP8 op w'

  cycle += 1

res :: Int -> OP -> CPU ()
res i op = do
  w <- readOP8 op
  let w' = clearBit w i
  writeOP8 op w'

  cycle += 1




dispatch :: CPU ()
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

    0x22 -> ld8_id_p_hl_a succ
    0x2a -> ld8_id_a_p_hl succ 
    0x32 -> ld8_id_p_hl_a pred
    0x3a -> ld8_id_a_p_hl pred

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
    0x10 -> stop

    0x00 -> nop 

    0xcb -> do
      instruction' <- readPC
      cycle += 1
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


getInstructionName :: CPU String
getInstructionName = do
  pc' <- use pc
  instruction <- read pc'
  case instruction of
    0x3e -> pure "ld8 A W"
    0x06 -> pure "ld8 B W"
    0x0e -> pure "ld8 C W"
    0x16 -> pure "ld8 D W"
    0x1e -> pure "ld8 E W"
    0x26 -> pure "ld8 H W"
    0x2e -> pure "ld8 L W"
    0x7f -> pure "ld8 A A"
    0x78 -> pure "ld8 A B"
    0x79 -> pure "ld8 A C"
    0x7a -> pure "ld8 A D"
    0x7b -> pure "ld8 A E"
    0x7c -> pure "ld8 A H"
    0x7d -> pure "ld8 A L"
    0x7e -> pure "ld8 A P_HL"
    0x0a -> pure "ld8 A P_BC"
    0x1a -> pure "ld8 A P_DE"
    0x47 -> pure "ld8 B A"
    0x40 -> pure "ld8 B B"
    0x41 -> pure "ld8 B C"
    0x42 -> pure "ld8 B D"
    0x43 -> pure "ld8 B E"
    0x44 -> pure "ld8 B H"
    0x45 -> pure "ld8 B L"
    0x46 -> pure "ld8 B P_HL"
    0x4f -> pure "ld8 C A"
    0x48 -> pure "ld8 C B"
    0x49 -> pure "ld8 C C"
    0x4a -> pure "ld8 C D"
    0x4b -> pure "ld8 C E"
    0x4c -> pure "ld8 C H"
    0x4d -> pure "ld8 C L"
    0x4e -> pure "ld8 C P_HL"
    0x57 -> pure "ld8 D A"
    0x50 -> pure "ld8 D B"
    0x51 -> pure "ld8 D C"
    0x52 -> pure "ld8 D D"
    0x53 -> pure "ld8 D E"
    0x54 -> pure "ld8 D H"
    0x55 -> pure "ld8 D L"
    0x56 -> pure "ld8 D P_HL"
    0x5f -> pure "ld8 E A"
    0x58 -> pure "ld8 E B"
    0x59 -> pure "ld8 E C"
    0x5a -> pure "ld8 E D"
    0x5b -> pure "ld8 E E"
    0x5c -> pure "ld8 E H"
    0x5d -> pure "ld8 E L"
    0x5e -> pure "ld8 E P_HL"
    0x67 -> pure "ld8 H A"
    0x60 -> pure "ld8 H B"
    0x61 -> pure "ld8 H C"
    0x62 -> pure "ld8 H D"
    0x63 -> pure "ld8 H E"
    0x64 -> pure "ld8 H H"
    0x65 -> pure "ld8 H L"
    0x66 -> pure "ld8 H P_HL"
    0x6f -> pure "ld8 L A"
    0x68 -> pure "ld8 L B"
    0x69 -> pure "ld8 L C"
    0x6a -> pure "ld8 L D"
    0x6b -> pure "ld8 L E"
    0x6c -> pure "ld8 L H"
    0x6d -> pure "ld8 L L"
    0x6e -> pure "ld8 L P_HL"
    0x70 -> pure "ld8 HL B"
    0x71 -> pure "ld8 HL C"
    0x72 -> pure "ld8 HL D"
    0x73 -> pure "ld8 HL E"
    0x74 -> pure "ld8 HL H"
    0x75 -> pure "ld8 HL L"

    0x36 -> pure "ld8 P_HL W"
    0x02 -> pure "ld8 P_BC A"
    0x12 -> pure "ld8 P_DE A"
    0x77 -> pure "ld8 P_HL A"
    0xea -> pure "ld8 P_WW A"

    0xf0 -> pure "ld8 A P_FF00_W"
    0xf2 -> pure "ld8 A P_FF00_C "
    0xfa -> pure "ld8 A P_WW"
    0xe0 -> pure "ld8 P_FF00_W A"
    0xe2 -> pure "ld8 P_FF00_C A"

    0x22 -> pure "ld8_id_p_hl_a succ"
    0x2a -> pure "ld8_id_a_p_hl succ "
    0x32 -> pure "ld8_id_p_hl_a pred"
    0x3a -> pure "ld8_id_a_p_hl pred"

    0x01 -> pure "ld16 BC WW"
    0x11 -> pure "ld16 DE WW"
    0x21 -> pure "ld16 HL WW"
    0x31 -> pure "ld16 SP WW"
    0xf9 -> pure "ld16 SP HL"
    0x08 -> pure "ld16 P_WW SP"
    0xf8 -> pure "ld16_hl_sp_w"

    0xf5 -> pure "push AF"
    0xc5 -> pure "push BC"
    0xd5 -> pure "push DE"
    0xe5 -> pure "push HL"
    0xf1 -> pure "pop AF"
    0xc1 -> pure "pop BC"
    0xd1 -> pure "pop DE"
    0xe1 -> pure "pop HL"

    0x87 -> pure "add A"
    0x80 -> pure "add B"
    0x81 -> pure "add C"
    0x82 -> pure "add D"
    0x83 -> pure "add E"
    0x84 -> pure "add H"
    0x85 -> pure "add L"
    0x86 -> pure "add P_HL "
    0xc6 -> pure "add W"

    0x8f -> pure "adc A"
    0x88 -> pure "adc B"
    0x89 -> pure "adc C"
    0x8a -> pure "adc D"
    0x8b -> pure "adc E"
    0x8c -> pure "adc H"
    0x8d -> pure "adc L"
    0x8e -> pure "adc P_HL "
    0xce -> pure "adc W"

    0x97 -> pure "sub A"
    0x90 -> pure "sub B"
    0x91 -> pure "sub C"
    0x92 -> pure "sub D"
    0x93 -> pure "sub E"
    0x94 -> pure "sub H"
    0x95 -> pure "sub L"
    0x96 -> pure "sub P_HL "
    0xd6 -> pure "sub W"

    0x9f -> pure "sbc A"
    0x98 -> pure "sbc B"
    0x99 -> pure "sbc C"
    0x9a -> pure "sbc D"
    0x9b -> pure "sbc E"
    0x9c -> pure "sbc H"
    0x9d -> pure "sbc L"
    0x9e -> pure "sbc P_HL "
    0xde -> pure "sbc W"

    0xa7 -> pure "and A"
    0xa0 -> pure "and B"
    0xa1 -> pure "and C"
    0xa2 -> pure "and D"
    0xa3 -> pure "and E"
    0xa4 -> pure "and H"
    0xa5 -> pure "and L"
    0xa6 -> pure "and P_HL "
    0xe6 -> pure "and W"

    0xb7 -> pure "or A"
    0xb0 -> pure "or B"
    0xb1 -> pure "or C"
    0xb2 -> pure "or D"
    0xb3 -> pure "or E"
    0xb4 -> pure "or H"
    0xb5 -> pure "or L"
    0xb6 -> pure "or P_HL "
    0xf6 -> pure "or W"

    0xaf -> pure "xor A"
    0xa8 -> pure "xor B"
    0xa9 -> pure "xor C"
    0xaa -> pure "xor D"
    0xab -> pure "xor E"
    0xac -> pure "xor H"
    0xad -> pure "xor L"
    0xae -> pure "xor P_HL "
    0xee -> pure "xor W"

    0xbf -> pure "cp A"
    0xb8 -> pure "cp B"
    0xb9 -> pure "cp C"
    0xba -> pure "cp D"
    0xbb -> pure "cp E"
    0xbc -> pure "cp H"
    0xbd -> pure "cp L"
    0xbe -> pure "cp P_HL "
    0xfe -> pure "cp W"

    0x3c -> pure "inc8 A"
    0x04 -> pure "inc8 B"
    0x0c -> pure "inc8 C"
    0x14 -> pure "inc8 D"
    0x1c -> pure "inc8 E"
    0x24 -> pure "inc8 H"
    0x2c -> pure "inc8 L"
    0x34 -> pure "inc8 P_HL "

    0x3d -> pure "dec8 A"
    0x05 -> pure "dec8 B"
    0x0d -> pure "dec8 C"
    0x15 -> pure "dec8 D"
    0x1d -> pure "dec8 E"
    0x25 -> pure "dec8 H"
    0x2d -> pure "dec8 L"
    0x35 -> pure "dec8 P_HL "

    0x09 -> pure "add_hl BC"
    0x19 -> pure "add_hl DE"
    0x29 -> pure "add_hl HL"
    0x39 -> pure "add_hl SP"
    0xe8 -> pure "add_sp"

    0x03 -> pure "inc16 BC"
    0x13 -> pure "inc16 DE"
    0x23 -> pure "inc16 HL"
    0x33 -> pure "inc16 SP"

    0x0b -> pure "dec16 BC"
    0x1b -> pure "dec16 DE"
    0x2b -> pure "dec16 HL"
    0x3b -> pure "dec16 SP"

    0x07 -> pure "rlc A"
    0x17 -> pure "rl A"
    0x0f -> pure "rrc A "
    0x1f -> pure "rr A "

    0x27 -> pure "daa "

    0x2f -> pure "cpl "
    0x3f -> pure "ccf "
    0x37 -> pure "scf "

    0xc3 -> pure "jp Always"
    0xc2 -> pure "jp NotZero"
    0xca -> pure "jp Zero"
    0xd2 -> pure "jp NotCarry"
    0xda -> pure "jp Carry"
    0xe9 -> pure "jp P_HL"
    0x18 -> pure "jr Always"
    0x20 -> pure "jr NotZero"
    0x28 -> pure "jr Zero"
    0x30 -> pure "jr NotCarry"
    0x38 -> pure "jr Carry"
    0xcd -> pure "call Always"
    0xc4 -> pure "call NotZero"
    0xcc -> pure "call Zero"
    0xd4 -> pure "call NotCarry"
    0xdc -> pure "call Carry"
    0xc7 -> pure "rst 0x00"
    0xcf -> pure "rst 0x08"
    0xd7 -> pure "rst 0x10"
    0xdf -> pure "rst 0x18"
    0xe7 -> pure "rst 0x20"
    0xef -> pure "rst 0x28"
    0xf7 -> pure "rst 0x30"
    0xff -> pure "rst 0x38"
    0xc9 -> pure "ret Always"
    0xc0 -> pure "ret NotZero"
    0xc8 -> pure "ret Zero"
    0xd0 -> pure "ret NotCarry"
    0xd8 -> pure "ret Carry"
    0xd9 -> pure "reti"

    0xf3 -> pure "di"
    0xfb -> pure "ei"

    0x76 -> pure "halt"
    0x10 -> pure "stop"

    0x00 -> pure "nop"

    0xcb -> do
      pc' <- use pc
      instruction' <- read (pc' + 1)
      case instruction' of 
        0x37 -> pure "swap A"
        0x30 -> pure "swap B"
        0x31 -> pure "swap C"
        0x32 -> pure "swap D"
        0x33 -> pure "swap E"
        0x34 -> pure "swap H"
        0x35 -> pure "swap L"
        0x36 -> pure "swap P_HL "

        0x07 -> pure "rlc A"
        0x00 -> pure "rlc B"
        0x01 -> pure "rlc C"
        0x02 -> pure "rlc D"
        0x03 -> pure "rlc E"
        0x04 -> pure "rlc H"
        0x05 -> pure "rlc L"
        0x06 -> pure "rlc P_HL "

        0x17 -> pure "rl A"
        0x10 -> pure "rl B"
        0x11 -> pure "rl C"
        0x12 -> pure "rl D"
        0x13 -> pure "rl E"
        0x14 -> pure "rl H"
        0x15 -> pure "rl L"
        0x16 -> pure "rl P_HL "

        0x0f -> pure "rrc A"
        0x08 -> pure "rrc B"
        0x09 -> pure "rrc C"
        0x0a -> pure "rrc D"
        0x0b -> pure "rrc E"
        0x0c -> pure "rrc H"
        0x0d -> pure "rrc L"
        0x0e -> pure "rrc P_HL "

        0x1f -> pure "rr A"
        0x18 -> pure "rr B"
        0x19 -> pure "rr C"
        0x1a -> pure "rr D"
        0x1b -> pure "rr E"
        0x1c -> pure "rr H"
        0x1d -> pure "rr L"
        0x1e -> pure "rr P_HL"

        0x27 -> pure "sla A"
        0x20 -> pure "sla B"
        0x21 -> pure "sla C"
        0x22 -> pure "sla D"
        0x23 -> pure "sla E"
        0x24 -> pure "sla H"
        0x25 -> pure "sla L"
        0x26 -> pure "sla P_HL"

        0x2f -> pure "sra A"
        0x28 -> pure "sra B"
        0x29 -> pure "sra C"
        0x2a -> pure "sra D"
        0x2b -> pure "sra E"
        0x2c -> pure "sra H"
        0x2d -> pure "sra L"
        0x2e -> pure "sra P_HL "

        0x3f -> pure "srl A"
        0x38 -> pure "srl B"
        0x39 -> pure "srl C"
        0x3a -> pure "srl D"
        0x3b -> pure "srl E"
        0x3c -> pure "srl H"
        0x3d -> pure "srl L"
        0x3e -> pure "srl P_HL "

        0x47 -> pure "bit 0 A"
        0x40 -> pure "bit 0 B"
        0x41 -> pure "bit 0 C"
        0x42 -> pure "bit 0 D"
        0x43 -> pure "bit 0 E"
        0x44 -> pure "bit 0 H"
        0x45 -> pure "bit 0 L"
        0x46 -> pure "bit 0 P_HL"
        0x4f -> pure "bit 1 A"
        0x48 -> pure "bit 1 B"
        0x49 -> pure "bit 1 C"
        0x4a -> pure "bit 1 D"
        0x4b -> pure "bit 1 E"
        0x4c -> pure "bit 1 H"
        0x4d -> pure "bit 1 L"
        0x4e -> pure "bit 1 P_HL"
        0x57 -> pure "bit 2 A"
        0x50 -> pure "bit 2 B"
        0x51 -> pure "bit 2 C"
        0x52 -> pure "bit 2 D"
        0x53 -> pure "bit 2 E"
        0x54 -> pure "bit 2 H"
        0x55 -> pure "bit 2 L"
        0x56 -> pure "bit 2 P_HL"
        0x5f -> pure "bit 3 A"
        0x58 -> pure "bit 3 B"
        0x59 -> pure "bit 3 C"
        0x5a -> pure "bit 3 D"
        0x5b -> pure "bit 3 E"
        0x5c -> pure "bit 3 H"
        0x5d -> pure "bit 3 L"
        0x5e -> pure "bit 3 P_HL"
        0x67 -> pure "bit 4 A"
        0x60 -> pure "bit 4 B"
        0x61 -> pure "bit 4 C"
        0x62 -> pure "bit 4 D"
        0x63 -> pure "bit 4 E"
        0x64 -> pure "bit 4 H"
        0x65 -> pure "bit 4 L"
        0x66 -> pure "bit 4 P_HL"
        0x6f -> pure "bit 5 A"
        0x68 -> pure "bit 5 B"
        0x69 -> pure "bit 5 C"
        0x6a -> pure "bit 5 D"
        0x6b -> pure "bit 5 E"
        0x6c -> pure "bit 5 H"
        0x6d -> pure "bit 5 L"
        0x6e -> pure "bit 5 P_HL"
        0x77 -> pure "bit 6 A"
        0x70 -> pure "bit 6 B"
        0x71 -> pure "bit 6 C"
        0x72 -> pure "bit 6 D"
        0x73 -> pure "bit 6 E"
        0x74 -> pure "bit 6 H"
        0x75 -> pure "bit 6 L"
        0x76 -> pure "bit 6 P_HL"
        0x7f -> pure "bit 7 A"
        0x78 -> pure "bit 7 B"
        0x79 -> pure "bit 7 C"
        0x7a -> pure "bit 7 D"
        0x7b -> pure "bit 7 E"
        0x7c -> pure "bit 7 H"
        0x7d -> pure "bit 7 L"
        0x7e -> pure "bit 7 P_HL"

        0xc7 -> pure "set 0 A"
        0xc0 -> pure "set 0 B"
        0xc1 -> pure "set 0 C"
        0xc2 -> pure "set 0 D"
        0xc3 -> pure "set 0 E"
        0xc4 -> pure "set 0 H"
        0xc5 -> pure "set 0 L"
        0xc6 -> pure "set 0 P_HL"
        0xcf -> pure "set 1 A"
        0xc8 -> pure "set 1 B"
        0xc9 -> pure "set 1 C"
        0xca -> pure "set 1 D"
        0xcb -> pure "set 1 E"
        0xcc -> pure "set 1 H"
        0xcd -> pure "set 1 L"
        0xce -> pure "set 1 P_HL"
        0xd7 -> pure "set 2 A"
        0xd0 -> pure "set 2 B"
        0xd1 -> pure "set 2 C"
        0xd2 -> pure "set 2 D"
        0xd3 -> pure "set 2 E"
        0xd4 -> pure "set 2 H"
        0xd5 -> pure "set 2 L"
        0xd6 -> pure "set 2 P_HL"
        0xdf -> pure "set 3 A"
        0xd8 -> pure "set 3 B"
        0xd9 -> pure "set 3 C"
        0xda -> pure "set 3 D"
        0xdb -> pure "set 3 E"
        0xdc -> pure "set 3 H"
        0xdd -> pure "set 3 L"
        0xde -> pure "set 3 P_HL"
        0xe7 -> pure "set 4 A"
        0xe0 -> pure "set 4 B"
        0xe1 -> pure "set 4 C"
        0xe2 -> pure "set 4 D"
        0xe3 -> pure "set 4 E"
        0xe4 -> pure "set 4 H"
        0xe5 -> pure "set 4 L"
        0xe6 -> pure "set 4 P_HL"
        0xef -> pure "set 5 A"
        0xe8 -> pure "set 5 B"
        0xe9 -> pure "set 5 C"
        0xea -> pure "set 5 D"
        0xeb -> pure "set 5 E"
        0xec -> pure "set 5 H"
        0xed -> pure "set 5 L"
        0xee -> pure "set 5 P_HL"
        0xf7 -> pure "set 6 A"
        0xf0 -> pure "set 6 B"
        0xf1 -> pure "set 6 C"
        0xf2 -> pure "set 6 D"
        0xf3 -> pure "set 6 E"
        0xf4 -> pure "set 6 H"
        0xf5 -> pure "set 6 L"
        0xf6 -> pure "set 6 P_HL"
        0xff -> pure "set 7 A"
        0xf8 -> pure "set 7 B"
        0xf9 -> pure "set 7 C"
        0xfa -> pure "set 7 D"
        0xfb -> pure "set 7 E"
        0xfc -> pure "set 7 H"
        0xfd -> pure "set 7 L"
        0xfe -> pure "set 7 P_HL"

        0x87 -> pure "res 0 A"
        0x80 -> pure "res 0 B"
        0x81 -> pure "res 0 C"
        0x82 -> pure "res 0 D"
        0x83 -> pure "res 0 E"
        0x84 -> pure "res 0 H"
        0x85 -> pure "res 0 L"
        0x86 -> pure "res 0 P_HL"
        0x8f -> pure "res 1 A"
        0x88 -> pure "res 1 B"
        0x89 -> pure "res 1 C"
        0x8a -> pure "res 1 D"
        0x8b -> pure "res 1 E"
        0x8c -> pure "res 1 H"
        0x8d -> pure "res 1 L"
        0x8e -> pure "res 1 P_HL"
        0x97 -> pure "res 2 A"
        0x90 -> pure "res 2 B"
        0x91 -> pure "res 2 C"
        0x92 -> pure "res 2 D"
        0x93 -> pure "res 2 E"
        0x94 -> pure "res 2 H"
        0x95 -> pure "res 2 L"
        0x96 -> pure "res 2 P_HL"
        0x9f -> pure "res 3 A"
        0x98 -> pure "res 3 B"
        0x99 -> pure "res 3 C"
        0x9a -> pure "res 3 D"
        0x9b -> pure "res 3 E"
        0x9c -> pure "res 3 H"
        0x9d -> pure "res 3 L"
        0x9e -> pure "res 3 P_HL"
        0xa7 -> pure "res 4 A"
        0xa0 -> pure "res 4 B"
        0xa1 -> pure "res 4 C"
        0xa2 -> pure "res 4 D"
        0xa3 -> pure "res 4 E"
        0xa4 -> pure "res 4 H"
        0xa5 -> pure "res 4 L"
        0xa6 -> pure "res 4 P_HL"
        0xaf -> pure "res 5 A"
        0xa8 -> pure "res 5 B"
        0xa9 -> pure "res 5 C"
        0xaa -> pure "res 5 D"
        0xab -> pure "res 5 E"
        0xac -> pure "res 5 H"
        0xad -> pure "res 5 L"
        0xae -> pure "res 5 P_HL"
        0xb7 -> pure "res 6 A"
        0xb0 -> pure "res 6 B"
        0xb1 -> pure "res 6 C"
        0xb2 -> pure "res 6 D"
        0xb3 -> pure "res 6 E"
        0xb4 -> pure "res 6 H"
        0xb5 -> pure "res 6 L"
        0xb6 -> pure "res 6 P_HL"
        0xbf -> pure "res 7 A"
        0xb8 -> pure "res 7 B"
        0xb9 -> pure "res 7 C"
        0xba -> pure "res 7 D"
        0xbb -> pure "res 7 E"
        0xbc -> pure "res 7 H"
        0xbd -> pure "res 7 L"
        0xbe -> pure "res 7 P_HL"

        _ -> error ("0xcb " ++ showHex instruction')
    _ -> error $ showHex instruction


