module StatusRegisters where

import qualified Data.Map.Strict as Map
import Data.Word

type Byte = Word8
type Halfword = Word16
type Word = Word32
type Address = Word32

type Register = Word32
type AddressSpace = Map.Map Address Byte

data GeneralPurposeRegisters = GeneralPurposeRegisters
  {
    -- Unbanked registers.
    r0 :: Register,
    r1 :: Register,
    r2 :: Register,
    r3 :: Register,
    r4 :: Register,
    r5 :: Register,
    r6 :: Register,
    r7 :: Register,

    -- Banked registers.
    r8        :: Register,
    r8_fiq    :: Register,
    r9        :: Register,
    r9_fiq    :: Register,
    r10       :: Register,
    r10_fiq   :: Register,
    r11       :: Register,
    r11_fiq   :: Register,
    r12       :: Register,
    r12_fiq   :: Register,
    r13       :: Register,
    r13_svc   :: Register,
    r13_abt   :: Register,
    r13_und   :: Register,
    r13_irq   :: Register,
    r13_fiq   :: Register,
    r14       :: Register,
    r14_svc   :: Register,
    r14_abt   :: Register,
    r14_und   :: Register,
    r14_irq   :: Register,
    r14_fiq   :: Register,

    -- Program counter.
    pc :: Register
  }

data StatusRegisters = StatusRegisters
  {
    conditionCodeFlags :: ConditionCodeFlags,
    irqInterruptMask :: Bool,
    fiqInterruptMask  :: Bool,
    thumbStateFlag :: Bool,
    processorMode :: ProcessorMode
  }

data ConditionCodeFlags = ConditionCodeFlags
  {
    n :: Bool,
    z :: Bool,
    c :: Bool,
    v :: Bool
  }

data ProcessorMode = User | FIQ | IRQ | Supervisor | Abort | Undefined | System
