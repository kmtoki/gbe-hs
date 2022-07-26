module Disassembler where

import Prelude hiding (read, cycle, log, or, and)

import Gameboy.Cartrige hiding (rom)
import Gameboy.Utils hiding (set,bit,xor)

import qualified System.IO as IO
import qualified Numeric as N
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as B

type Disassembler a = StateT DisassemblerState IO a

data DisassemblerState = DisassemblerState {
    _rom :: V.Vector Word8,
    _buffer :: VM.IOVector String,
    _pos :: Int,
    _pc :: Int,
    _pc' :: Int
  }

makeLenses ''DisassemblerState

data OP
  = A | F | B | C | D | E | H | L
  | AF | BC | DE | HL | SP | PC
  | P_BC | P_DE | P_HL | P_WW
  | P_FF00_C | P_FF00_WW
  | W | WW
  | Zero | NotZero | Carry | NotCarry | Always
  | X | Info String

instance Show OP where
  show op = case op of
    A -> "A"
    F -> "F"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"
    H -> "H"
    L -> "L"
    AF -> "AF"
    BC -> "BC"
    DE -> "DE"
    HL -> "HL"
    SP -> "SP"
    PC -> "SP"
    P_BC -> "(BC)"
    P_DE -> "(DE)"
    P_HL -> "(HL)"
    P_WW -> "(nn)"
    P_FF00_C -> "($FF00+C)"
    P_FF00_WW -> "($FF00+nn)"
    W -> "n"
    WW -> "nn"
    Zero -> "Z"
    NotZero -> "NZ"
    Carry -> "C"
    NotCarry -> "NC"
    Always -> "_"
    X -> ""
    Info s -> s

newDisassemblerState :: ROM -> IO DisassemblerState
newDisassemblerState rom = do
  bf <- VM.replicate (V.length rom + 1) "XXXX"
  pure $ DisassemblerState {
      _rom = rom,
      _buffer = bf,
      _pos = 0,
      _pc = 0,
      _pc' = 0
    }

read :: Address i => i -> Disassembler Word8
read i = do
  rom' <- use rom
  pure $ (rom' V.! (toInt i))

readPC :: Disassembler Word8
readPC = do
  pc' <- use pc
  w <- read pc'
  pc += 1
  pure w

readOP :: OP -> Disassembler String
readOP op = case op of
  P_WW -> do
    --pc += 2
    ww <- flip toWW <$> readPC <*> readPC
    pure $ ("(" ++ showHex ww ++ ")")
  P_FF00_WW -> do
    --pc += 2
    ww <- flip toWW <$> readPC <*> readPC
    pure $ ("($FF00+" ++ showHex ww ++ ")")
  W -> do
    --pc += 1
    w <- readPC
    pure $ showHex w
  WW -> do
    --pc += 2
    ww <- flip toWW <$> readPC <*> readPC
    pure $ showHex ww
  _ -> pure $ show op


showPC :: Int -> String
showPC x = num ++ hex
  where
    hex = N.showHex x ""
    len = length hex
    width = 8
    off = width - len
    num = replicate (if off > 0 then off else 0) '0' 

emit :: String -> OP -> OP -> Disassembler ()
emit instr op1 op2 = do
  pc'' <- use pc'
  s1 <- readOP op1
  s2 <- readOP op2
  let assem = (showPC pc'' ++ ": " ++ instr ++ " " ++ s1 ++ " " ++ s2)
  p <- use pos
  b <- use buffer
  VM.write b p assem
  pos += 1

nop :: Disassembler ()
nop = do
  emit "nop" X X

ld8 :: OP -> OP -> Disassembler ()
ld8 op1 op2 = do
  emit "ld8" op1 op2 

ld16 :: OP -> OP -> Disassembler ()
ld16 op1 op2 = do
  emit "ld16" op1 op2 

ld8_id_a_p_hl :: String -> Disassembler ()
ld8_id_a_p_hl s = do
  emit ("ld8" ++ s) A P_HL

ld8_id_p_hl_a :: String -> Disassembler ()
ld8_id_p_hl_a s = do
  emit ("ld8" ++ s) P_HL A

ld16_hl_sp_w :: Disassembler ()
ld16_hl_sp_w = do
  w <- readPC
  emit "ld16" HL $ Info ("SP + " ++ show (fi w :: Int8))

push :: OP -> Disassembler ()
push op = do
  emit "push" op X

pop :: OP -> Disassembler ()
pop op = do
  emit "pop" op X

add :: OP -> Disassembler ()
add op = do
  emit "add" A op

adc :: OP -> Disassembler ()
adc op = do
  emit "adc" A op

sub :: OP -> Disassembler ()
sub op = do
  emit "sub" A op

sbc :: OP -> Disassembler ()
sbc op = do
  emit "sbc" A op


and :: OP -> Disassembler ()
and op = do
  emit "and" A op

or :: OP -> Disassembler ()
or op = do
  emit "or" A op

xor :: OP -> Disassembler ()
xor op = do
  emit "xor" A op


cp :: OP -> Disassembler ()
cp op = do
  emit "cp" A op

inc8 :: OP -> Disassembler ()
inc8 op = do
  emit "inc8" op X

dec8 :: OP -> Disassembler ()
dec8 op = do
  emit "dnc8" op X

add_hl :: OP -> Disassembler ()
add_hl op = do
  emit "add16" HL op

add_sp :: Disassembler ()
add_sp = do
  w <- readPC
  emit "add16" SP $ Info $ show $ (fi w :: Int8)

inc16 :: OP -> Disassembler ()
inc16 op = do
  emit "inc16" op X

dec16 :: OP -> Disassembler ()
dec16 op = do
  emit "dnc16" op X

daa :: Disassembler ()
daa = do
  emit "daa" X X

cpl :: Disassembler ()
cpl = do
  emit "cpl" (Info "#not A") X
      
ccf :: Disassembler ()
ccf = do
  emit "cff" (Info "#not carry") X

scf :: Disassembler ()
scf = do
  emit "scf" (Info "#set carry") X

halt :: Disassembler ()
halt = do
  emit "halt" X X

stop :: Disassembler ()
stop = do
  emit "stop" X X

di :: Disassembler ()
di = do
  emit "di" X X

ei :: Disassembler ()
ei = do
  emit "ei" X X

jp :: OP -> Disassembler ()
jp op = do
  emit "jp" op WW

jr :: OP -> Disassembler ()
jr op = do
  i <- readPC
  emit "jr" op $ Info $ show (fi i :: Int8)

call :: OP -> Disassembler ()
call op = do
  emit "call" op WW

rst :: Word16 -> Disassembler ()
rst ww = do
  emit "rst" (Info $ showHex ww) X
 
reti :: Disassembler ()
reti = do
  emit "reti" X X

ret :: OP -> Disassembler ()
ret op = do
  emit "ret" op X

swap :: OP -> Disassembler () 
swap op = do
  emit "swap" op X

rlc :: OP -> Disassembler ()
rlc op = do
  emit "rlc" op X

rl :: OP -> Disassembler ()
rl op = do
  emit "rl" op X

rrc :: OP -> Disassembler ()
rrc op = do
  emit "rrc" op X

rr :: OP -> Disassembler ()
rr op = do
  emit "rr" op X

sla :: OP -> Disassembler ()
sla op = do
  emit "sla" op X

sra :: OP -> Disassembler ()
sra op = do
  emit "sra" op X

srl :: OP -> Disassembler ()
srl op = do
  emit "srl" op X

bit :: Int -> OP -> Disassembler ()
bit i op = do
  emit "bit" op (Info $ show i)

set :: Int -> OP -> Disassembler ()
set i op = do
  emit "set" op (Info $ show i)

res :: Int -> OP -> Disassembler ()
res i op = do
  emit "res" op (Info $ show i)

dispatch :: Disassembler ()
dispatch = do
  pc' <~ use pc
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

    0xf0 -> ld8 A P_FF00_WW
    0xf2 -> ld8 A P_FF00_C 
    0xfa -> ld8 A P_WW
    0xe0 -> ld8 P_FF00_WW A
    0xe2 -> ld8 P_FF00_C A

    0x22 -> ld8_id_p_hl_a "++"
    0x2a -> ld8_id_a_p_hl "++"
    0x32 -> ld8_id_p_hl_a "--"
    0x3a -> ld8_id_a_p_hl "--"

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

        --x -> emit "CB XXX" (Info $ showHex x) X
        _ -> pure ()
    --x -> emit "XXX" (Info $ showHex x) X
    _ -> pure ()

executeDisassembler = do
  pc' <- use pc
  len <- V.length <$> use rom
  when (pc' < len) $ do
    dispatch
    executeDisassembler

main' = do
  rom' <- readFileROM "roms/gb_test_roms/cpu_instrs/cpu_instrs.gb"
  d <- newDisassemblerState rom'
  d' <- execStateT executeDisassembler d

  fh <- IO.openFile "disasm/cpu_instrs.gb" IO.WriteMode
  VM.forM_ (d'^.buffer) $ \v -> do
    IO.hPutStrLn fh v
  IO.hClose fh
