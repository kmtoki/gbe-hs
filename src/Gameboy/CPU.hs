module Gameboy.CPU where

import Prelude hiding (read, cycle, log, or, and)

import Gameboy.MBC
import Gameboy.Logger
import Gameboy.Utils hiding (set,bit,xor,modify)

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
    --__zero, __negative, __half, __carry :: Bool,
    _serial_counter :: Word8,
    _serial_buffer :: V.Vector Word8,
    _sys_counter :: Word16,
    _ime :: Bool,
    --_ime_prev :: Bool,
    _halting :: Bool,
    _stoping :: Bool,
    _cycle :: Int,
    _exe_counter :: Word64
  } deriving Show

makeLenses ''CPUState

data OP
  = A | F | B | C | D | E | H | L
  | A_
  | AF | BC | DE | HL | SP 
  | P_BC | P_DE | P_HL | P_WW
  | P_FF00_C | P_FF00_W
  | W | WW
  | Zero | NotZero | Carry | NotCarry | Always
  deriving (Show,Eq)

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
  _exe_counter = 1
  }

f :: Lens' CPUState Word8
f = lens get set
  where
    get = __f
    set c w = c { __f = w .&. 0b11110000 } -- ignore/reset lower 4 bits always

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
        __f'' = __f' .&. 0b11110000

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
read i = do
  cycle += 1
  lift $ do
    reader' <- use reader
    a <- reader' $ toInt i
    --lift $ logging 1 ("Read: " ++ showHex (toInt i) ++ " -> " ++ showHex a)
    pure a

write :: Address i => i -> Word8 -> CPU ()
write i w = do
  cycle += 1
  lift $ do
    writer' <- use writer
    --lift $ logging 1 ("Writ: " ++ showHex (toInt i) ++ " <- " ++ showHex w)
    writer' (toInt i) w

modify :: Address i => i -> (Word8 -> Word8) -> CPU ()
modify i f = do
  x <- read i
  write i $ f x

log :: Int -> String -> CPU ()
log n s = lift $ lift $ logging n s

push' :: Word8 -> CPU ()
push' i = do
  sp -= 1
  sp' <- use sp 
  cycle += 1
  write sp' i

pop' :: CPU Word8
pop' = do
  sp' <- use sp
  w <- read sp'
  sp += 1
  cycle += 1
  pure w

push16 :: Word16 -> CPU ()
push16 ww = do
  let (h',l') = sepWW ww
  push' h'
  push' l'

pop16 :: CPU Word16
pop16 = do
  l' <- pop'
  h' <- pop'
  pure $ toWW h' l'

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

logCPUState :: CPU ()
logCPUState = do
  logger <- lift $ lift $ get
  when (logger^.isLogging) $ do
    cpu' <- get
    (Just bank') <- lift $ preuse $ mbcnState.bank
    e <- use exe_counter
    p <- use pc
    s <- use sp
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
        ++ (showCPUState cpu')
    log 3 str

executeCPU :: CPU ()
executeCPU = do
  cycle .= 0

  logCPUState

  halt' <- use halting
  if halt' then
    --nop
    cycle += 1
  else do
    dispatch
    exe_counter += 1

  --let 
  --  loop :: Monad m => Int -> (Int -> m Bool) -> m ()
  --  loop n m = do
  --    b <- m n
  --    if b then
  --      loop (n + 1) m
  --    else
  --      pure ()

  --loop 0 $ \n -> do
  --  cycle' <- use cycle
  --  if n < cycle' then do
  --    serial
  --    timer
  --    interrupt
  --    sys_counter += 1
  --    pure True
  --  else
  --    pure False

  cycle' <- use cycle
  forM_ [1 .. cycle' * 4] $ \i -> do
    serial
    timer
    interrupt
    sys_counter += 1




timer :: CPU ()
timer = do
  ssc <- use sys_counter

  when (ssc `mod` 256 == 0) $ do
    modify DIV (+ 1)

  tac <- read TAC
  when (testBit tac 2) $ do
    let clock = [1024,16,64,256] !! fi (tac .&. 0b11)
    when (ssc `mod` clock == 0) $ do
      tima <- read TIMA
      let (tima',carry',_) = add8WithCarryHalf tima 1
      if carry' then do
        modify IF (flip setBit 2)
        tma <- read TMA
        write TIMA tma
        --log 4 ("Timer: clock:" ++ show clock ++ " tma:" ++ showHex tma)
      else
        write TIMA tima'

serial :: CPU ()
serial = do
  sc <- read SC
  when (testBit sc 7) $ do
    let clock = [512, 256, 16, 8] !! fi (sc .&. 0b11) -- 8192, 16384, 262144, 524288
    ssc <- use sys_counter
    when (ssc `mod` clock == 0) $ do
      sb <- read SB
      serial_buffer %= (flip V.snoc sb)
      sbs <- use serial_buffer
      --log 4 ("Serial: " ++ (V.toList $ V.map (chr.fi) sbs))

      write SC $ clearBit sc 7
      modify IF (flip setBit 3)

interrupt :: CPU ()
interrupt = do
  master <- use ime
  enable <- read IE
  request <- read IF

  when (enable .&. request /= 0) $ do
      halting .= False

  when master $ do
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
    when (addr /= 0) $ do
      pc' <- use pc
      push16 pc'
      write IF $ clearBit request bit
      pc .= addr
      ime .= False
      halting .= False
      cycle += 3
      --log 4 ("Interrupt: " ++ cate ++ " from " ++ showHex pc')


add8WithCarryHalf :: Word8 -> Word8 -> (Word8, Bool, Bool)
add8WithCarryHalf w1 w2 = (w3, c', h')
  where
    w3 = w1 + w2
    c' = w1 > w3
    h' = (w1 .^. w2 .^. w3) .&. 0x10 /= 0

sub8WithCarryHalf :: Word8 -> Word8 -> (Word8, Bool, Bool)
sub8WithCarryHalf w1 w2 = (w3, c', h')
  where
    w3 = w1 - w2
    c' = w1 < w3
    h' = (w1 .^. w2 .^. w3) .&. 0x10 /= 0

add16WithCarryHalf :: Word16 -> Word16 -> (Word16, Bool, Bool)
add16WithCarryHalf w1 w2 = (w3, c', h')
  where
    w3 = w1 + w2 
    c' = w1 > w3
    h' = (w1 .^. w2 .^. w3) .&. 0x1000 /= 0

sub16WithCarryHalf :: Word16 -> Word16 -> (Word16, Bool, Bool)
sub16WithCarryHalf w1 w2 = (w3, c', h')
  where
    w3 = w1 - w2
    c' = w1 > w3
    h' = (w1 .^. w2 .^. w3) .&. 0x1000 /= 0

addWord16SignedWord8WithCarryHalf :: Word16 -> Word8 -> (Word16, Bool, Bool)
addWord16SignedWord8WithCarryHalf a b = (r, c', h')
  where
    i = fi (fi b :: Int8) :: Int32
    u = fi i :: Word16
    r = fi ((fi a :: Int32) + i) :: Word16
    c' = (a .^. u .^. r) .&. 0x100 /= 0
    h' = (a .^. u .^. r) .&. 0x10 /= 0
  

readPC :: CPU Word8
readPC = do
  pc' <- use pc
  w <- read pc'
  pc += 1
  cycle += 1
  pure w

readOP8 :: OP -> CPU Word8
readOP8 op = do
  case op of
    A -> use a
    A_ -> use a
    F -> use f
    B -> use b
    C -> use c
    D -> use d
    E -> use e
    H -> use h
    L -> use l
    W -> readPC
    P_BC -> use bc >>= read
    P_DE -> use de >>= read
    P_HL -> use hl >>= read
    P_WW -> (flip toWW <$> readPC <*> readPC) >>= read . toInt
    P_FF00_C -> use c >>= read . (0xff00 +) . toInt
    P_FF00_W -> readPC >>= read . (0xff00 +) . toInt

writeOP8 :: OP -> Word8 -> CPU ()
writeOP8 op w = do
  case op of
    A -> a .= w
    A_ -> a .= w
    F -> f .= w
    B -> b .= w
    C -> c .= w
    D -> d .= w
    E -> e .= w
    H -> h .= w
    L -> l .= w
    P_BC -> use bc >>= flip write w
    P_DE -> use de >>= flip write w
    P_HL -> use hl >>= flip write w
    P_WW -> (flip toWW <$> readPC <*> readPC) >>= flip write w
    P_FF00_C -> use c >>= flip write w . (0xff00 +) . toInt
    P_FF00_W -> readPC >>= flip write w . (0xff00 +) . toInt
    _ -> error $ show op

readOP16 :: OP -> CPU Word16
readOP16 op = do
  case op of
    AF -> use af
    BC -> use bc
    DE -> use de
    HL -> use hl
    SP -> use sp
    WW -> (flip toWW <$> readPC <*> readPC)

writeOP16 :: OP -> Word16 -> CPU ()
writeOP16 op ww = do
  case op of
    AF -> af .= ww
    BC -> bc .= ww
    DE -> de .= ww
    HL -> hl .= ww
    SP -> sp .= ww
    P_WW -> do 
      addr <- flip toWW <$> readPC <*> readPC
      let (h', l') = sepWW ww
      write addr l'
      write (addr + 1) h'
      --cycle -= 1

cond :: OP -> CPU Bool
cond op = do
  case op of
    NotZero -> not <$> use zero
    Zero -> use zero 
    NotCarry -> not <$> use carry
    Carry -> use carry
    Always -> pure True

logI :: (Show a, Show b, Show c) => String -> a -> b -> c -> CPU ()
logI instr o1 o2 o3 = log 3 ("> " ++ instr ++ " " ++ show o1 ++ " " ++ show o2 ++ " " ++ show o3)

nop :: CPU ()
nop = do
  log 3 "NOP"

ld8 :: OP -> OP -> CPU ()
ld8 op1 op2 = do
  w <- readOP8 op2
  writeOP8 op1 w

  logI "LD" op1 op2 (Rst $ showHex w)

ld16 :: OP -> OP -> CPU ()
ld16 op1 op2 = do
  ww <- readOP16 op2
  writeOP16 op1 ww

  logI "LD" op1 op2 (Rst $ showHex ww)

ld8_id_a_p_hl :: (Word16 -> Word16) -> String -> CPU ()
ld8_id_a_p_hl f s = do
  hl' <- use hl
  a <~ read hl'
  hl %= f

  logI s A P_HL Non

ld8_id_p_hl_a :: (Word16 -> Word16) -> String -> CPU ()
ld8_id_p_hl_a f s = do
  hl' <- use hl
  a' <- use a
  write hl' a'
  hl %= f

  logI s P_HL A Non

ld16_hl_sp_w :: CPU ()
ld16_hl_sp_w = do
  w <- readPC
  sp' <- use sp 
  let (sp'',c',h') = addWord16SignedWord8WithCarryHalf sp' w
  hl .= sp''

  zero .= False
  negative .= False
  half .= h'
  carry .= c'
  cycle += 1
  logI "LD" HL SP $ Signed8 w

push :: OP -> CPU ()
push op = do
  ww <- readOP16 op
  push16 ww
  cycle += 1

  logI "PUSH" op (showHex ww) Non

pop :: OP -> CPU ()
pop op = do
  ww <- pop16
  writeOP16 op ww

  logI "POP" op (showHex ww) Non

add :: OP -> CPU ()
add op = do
  a' <- use a
  w <- readOP8 op
  let (a'', c', h') = add8WithCarryHalf a' w
  a .= a''

  zero .= isZero a''
  negative .= False
  half .= h'
  carry .= c'
  logI "ADD" op (Rst $ showHex w) ""

adc :: OP -> CPU ()
adc op = do
  a' <- use a
  w <- readOP8 op
  c' <- use carry
  let 
    (a'', c'', h'') = add8WithCarryHalf a' w
    (a''', c''', h''') = add8WithCarryHalf a'' $ toNum c'
  a .= a'''

  zero .= isZero a'''
  negative .= False
  half .= (h'' || h''')
  carry .= (c'' || c''')
  logI "ADC" op (Rst $ showHex w) Non

sub :: OP -> CPU ()
sub op = do
  a' <- use a
  w <- readOP8 op
  let (a'', c', h') = sub8WithCarryHalf a' w
  a .= a''

  zero .= isZero a''
  negative .= True
  half .= h'
  carry .= c'

  logI "SUB" op (Rst $ showHex w) Non

sbc :: OP -> CPU ()
sbc op = do
  a' <- use a
  w <- readOP8 op
  c' <- use carry
  let 
    (a'', c'', h'') = sub8WithCarryHalf a' w
    (a''', c''', h''') = sub8WithCarryHalf a'' $ toNum c'
  a .= a'''

  zero .= isZero a'''
  negative .= True
  half .= (h'' || h''')
  carry .= (c'' || c''')

  logI "SBC" op (Rst $ showHex w) Non


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

  logI "AND" op (Rst $ showHex w) Non

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

  logI "OR" op (Rst $ showHex w) Non

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

  logI "XOR" op (Rst $ showHex w) Non


cp :: OP -> CPU ()
cp op = do
  a' <- use a
  w <- readOP8 op
  let (a'', c', h') = sub8WithCarryHalf a' w

  zero .= isZero a''
  negative .= True
  half .= h'
  carry .= c'

  logI "CP" op (Rst $ showHex w) Non

inc8 :: OP -> CPU ()
inc8 op = do
  w <- readOP8 op
  let (w', _, h') = add8WithCarryHalf w 1
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= h'

  logI "INC" op (Rst $ showHex w') Non

dec8 :: OP -> CPU ()
dec8 op = do
  w <- readOP8 op
  let (w', _, h') = sub8WithCarryHalf w 1
  writeOP8 op w'

  zero .= isZero w'
  negative .= True
  half .= h'

  logI "DEC" op (Rst $ showHex w') Non

add_hl :: OP -> CPU ()
add_hl op = do
  hl' <- use hl
  ww <- readOP16 op
  let (hl'',carry',half') = add16WithCarryHalf hl' ww
  hl .= hl''

  negative .= False
  half .= half'
  carry .= carry'

  logI "ADD" HL op $ Rst $ showHex ww

add_sp :: CPU ()
add_sp = do
  sp' <- use sp
  w <- readPC
  let (sp'', c', h') = addWord16SignedWord8WithCarryHalf sp' w
  sp .= sp''

  zero .= False
  negative .= False
  half .= h'
  carry .= c'
  cycle += 2

  logI "ADD" SP w $ showSignedWord8 w

inc16 :: OP -> CPU ()
inc16 op = do
  ww <- readOP16 op
  let (ww', _, _) = add16WithCarryHalf ww 1
  writeOP16 op ww'

  logI "INC" op (Rst $ showHex ww) Non

dec16 :: OP -> CPU ()
dec16 op = do
  ww <- readOP16 op
  let (ww', _, _) = sub16WithCarryHalf ww 1
  writeOP16 op ww'
  
  logI "DEC" op (Rst $ showHex ww) Non

daa :: CPU ()
daa = do
  a' <- use a
  negative' <- use negative
  half' <- use half
  carry' <- use carry
   
  let 
    adjust = (if carry' then 0x60 else 0) .|. (if half' then 0x06 else 0)
    adjust' = 
      if not negative' then
        adjust .|. (if a' > 0x99 then 0x60 else 0) .|. (if a' .&. 0x0f > 0x09 then 0x06 else 0)
      else
        0
    res = if not negative' then a' + adjust' else a' - adjust'

  a .= res
  carry .= (adjust >= 0x60)
  zero .= isZero res
  half .= False

  logI "DAA" (Rst $ showHex res) Non Non

cpl :: CPU ()
cpl = do
  a %= complement

  negative .= True
  half .= True

  logI "CPL" A Non Non
      
ccf :: CPU ()
ccf = do
  carry %= not

  negative .= False
  half .= False

  logI "CCF" Carry Non Non

scf :: CPU ()
scf = do
  carry .= True

  negative .= False
  half .= False

  logI "SCF" Carry Non Non

halt :: CPU ()
halt = do
  halting .= True

  logI "HALT" Non Non Non

stop :: CPU ()
stop = do
  stoping .= True

  logI "STOP" Non Non Non

di :: CPU ()
di = do
  ime .= False

  logI "DI" Non Non Non

ei :: CPU ()
ei = do
  ime .= True

  logI "EI" Non Non Non

jp :: OP -> CPU ()
jp op = do
  ww <- readOP16 WW
  pc' <- use pc
  bool <- cond op
  when bool $ do
    pc .= ww
    cycle += 1

  pc'' <- use pc
  if pc' == pc'' then
    logI "JP" op (showHex ww) $ Info "No Jump"
  else
    logI "JP" op (showHex pc'') Non
    
jp_p_hl :: CPU ()
jp_p_hl = do
  pc <~ use hl
  cycle += 1
  pc' <- use pc
  logI "JP" P_HL (showHex pc') Non
  

jr :: OP -> CPU ()
jr op = do
  i <- readPC
  pc' <- use pc
  let (pc'', _, _) = addWord16SignedWord8WithCarryHalf pc' i
  bool <- cond op
  when bool $ do
    pc .= pc''
    cycle += 1

  pc''' <- use pc
  if pc' == pc''' then
    logI "JR" op (showSignedWord8 i) $ Info "No Jump"
  else
    logI "JR" op (showSignedWord8 i) (showHex pc''')

call :: OP -> CPU ()
call op = do
  ww <- readOP16 WW
  pc' <- use pc
  bool <- cond op
  when bool $ do
    push16 pc'
    pc .= ww
    cycle += 1

  pc'' <- use pc
  if pc' == pc'' then
    logI "CALL" op (showHex ww) $ Info "No Call"
  else
    logI "CALL" op (showHex ww) Non



rst :: Word16 -> CPU ()
rst ww = do
  pc' <- use pc
  push16 pc'
  pc .= ww
  cycle += 1

  cycle .= 8
  logI "RST" (showHex ww) Non Non
 
reti :: CPU ()
reti = do
  ww <- pop16
  pc .= ww
  ime .= True
  cycle += 1


  pc' <- use pc
  logI "RETI" (showHex pc') Non Non

ret :: OP -> CPU ()
ret op = do
  pc' <- use pc
  bool <- cond op
  when bool $ do
    ww <- pop16
    pc .= ww
    cycle += 1

  pc'' <- use pc
  if pc' == pc'' then
    logI "RET" op (showHex pc') $ Info "No Return"
  else
    logI "RET" op (showHex pc'') Non



swap :: OP -> CPU () 
swap op = do
  w <- readOP8 op
  let w' =  shift w 4 .|. shift w (-4)
  writeOP8 op w'

  zero .= isZero w'
  negative .= False
  half .= False
  carry .= False

  logI "SWAP" op (Rst $ showHex w') Non

rlc :: OP -> CPU ()
rlc op = do
  w <- readOP8 op
  let 
    carry' = w `shiftR` 7
    w' = w `shiftL` 1 .|. carry'
  writeOP8 op w' 

  carry .= (carry' == 1)
  zero .= isZero w'
  negative .= False
  half .= if op == A_ then False else isZero w'

  logI "RLC" op (Rst $ showHex w') Non

rl :: OP -> CPU ()
rl op = do
  w <- readOP8 op
  carry' <- use carry
  let 
    w' = shift w 1 .|. toNum carry'
  writeOP8 op w'

  carry .= (1 == w `shiftR` 7)
  negative .= False
  half .= False
  zero .= if op == A_ then False else isZero w'

  logI "RL" op (Rst $ showHex w') Non

rrc :: OP -> CPU ()
rrc op = do
  w <- readOP8 op
  let 
    carry' = w .&. 1
    w' = carry' `shiftL` 7 .|. w `shiftR` 1
  writeOP8 op w'

  carry .= (carry' == 1)
  negative .= False
  half .= False
  zero .= if op == A_ then False else isZero w'

  logI "RRC" op (Rst $ showHex w') Non

rr :: OP -> CPU ()
rr op = do
  w <- readOP8 op
  carry' <- use carry
  let w' = shift (toNum carry') 7 .|. shift w (-1)
  writeOP8 op w'

  carry .= (1 == (w' .&. 1))
  negative .= False
  half .= False
  zero .= if op == A_ then False else isZero w'

  logI "RR" op (Rst $ showHex w') Non

sla :: OP -> CPU ()
sla op = do
  w <- readOP8 op
  let w' = shift w 1
  writeOP8 op w'

  carry .= (1 == shift w (-7))
  negative .= False
  half .= False
  zero .= isZero w'

  logI "SLA" op (Rst $ showHex w') Non

sra :: OP -> CPU ()
sra op = do
  w <- readOP8 op
  let w' = (w .&. 0b10000000) .|. shift w (-1)
  writeOP8 op w'

  carry .= (1 == (w .&. 1))
  zero .= isZero w'
  negative .= False
  half .= False

  logI "SRA" op (Rst $ showHex w') Non

srl :: OP -> CPU ()
srl op = do
  w <- readOP8 op
  let w' = w `shiftR` 1
  writeOP8 op w'

  carry .= ((w .&. 1) == 1)
  half .= False
  negative .= False
  zero .= isZero w'

  logI "SRL" op (Rst $ showHex w') Non

bit :: Int -> OP -> CPU ()
bit i op = do
  w <- readOP8 op
  let w' = testBit w i

  half .= True
  negative .= False
  zero .= (w' == False)

  logI "BIT" i op (Rst $ show w')

set :: Int -> OP -> CPU ()
set i op = do
  w <- readOP8 op
  let w' = setBit w i
  writeOP8 op w'

  logI "SET" i op (Rst $ showHex w')

res :: Int -> OP -> CPU ()
res i op = do
  w <- readOP8 op
  let w' = clearBit w i
  writeOP8 op w'

  logI "RES" i op (Rst $ showHex w')




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
    0xca -> jp Zero
    0xd2 -> jp NotCarry
    0xda -> jp Carry
    0xe9 -> jp_p_hl
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

    0x10 -> do
      instruction' <- readPC
      case instruction' of 
        0x00 -> stop
        _ -> error $ "CPU undefind instruction 0x10 " ++ showHex instruction'

    0xcb -> do
      instruction' <- readPC
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
