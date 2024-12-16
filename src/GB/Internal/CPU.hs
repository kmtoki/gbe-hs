module GB.Internal.CPU (
  CPUFlags(..),
  CPURegisters8(..),
  CPURegisters16(..),
  CPURegisters64(..),
  CPUOp8(..),
  CPUOp16(..),
  CPUOpCond(..),
  CPULog(..),
  CPUOpLog(..)
  ) where

import GB.Prelude

data CPUFlags = Carry | Half | Negative | Zero
  deriving (Enum, Show)

data CPURegisters8 = A | F | B | C | D | E | H | L | IME | Halt | Cycle | IsLogging
  deriving (Enum, Show, Eq)

data CPURegisters16 = SP | PC
  deriving (Enum, Show)

data CPURegisters64 = SysCounter | ExeCounter
  deriving (Enum, Show)

data CPUOp8 
  = Reg8 CPURegisters8
  | A_
  | N
  | P_BC | P_DE | P_HL | P_NN_8
  | P_FF00_N | P_FF00_C
  | P_HL_INC
  | P_HL_DEC
  deriving (Show, Eq)

data CPUOp16
  = Reg16 CPURegisters16
  | AF | BC | DE | HL
  | NN
  | P_NN_16
  deriving Show

data CPUOpCond
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
  = Log8 CPUOp8 | Log16 CPUOp16 | LogCond CPUOpCond 
  | LogInfo String
  | LogInfoW8 Word8 | LogInfoW16 Word16 | LogInfoI8 Int8 
  | None

