module Types ( Immediate, Register, Flag, Bit, Execute
             , ProcessorMode(..), CPU(..)
             , Instruction(..), RawInstruction(..)
             , Shifter(..), AddressingModeType(..)
             , AddressingMode2(..), AddressingMode3(..)
             , AddressingMode4(..)
             ) where

import Data.Word
import Imperative
import Memory -- TODO: Decouple memory from CPU

-- Specializations of Value to CPU specific types
type Immediate a = Value CPU a 
type Register = Value CPU Word32
type Flag = Value CPU Bool
type Bit = Value CPU Word32
type Execute = Value CPU CPU

-- CPU types
data ProcessorMode = User
                   | FIQ
                   | IRQ
                   | Supervisor
                   | Abort
                   | Undefined
                   | System
  deriving (Eq, Show)

data CPU = CPU {
  cpu_r0        :: Word32,
  cpu_r1        :: Word32,
  cpu_r2        :: Word32,
  cpu_r3        :: Word32,
  cpu_r4        :: Word32,
  cpu_r5        :: Word32,
  cpu_r6        :: Word32,
  cpu_r7        :: Word32,
  cpu_r8        :: Word32,
  cpu_r8_fiq    :: Word32,
  cpu_r9        :: Word32,
  cpu_r9_fiq    :: Word32,
  cpu_r10       :: Word32,
  cpu_r10_fiq   :: Word32,
  cpu_r11       :: Word32,
  cpu_r11_fiq   :: Word32,
  cpu_r12       :: Word32,
  cpu_r12_fiq   :: Word32,
  cpu_r13       :: Word32,
  cpu_r13_abt   :: Word32,
  cpu_r13_fiq   :: Word32,
  cpu_r13_irq   :: Word32,
  cpu_r13_svc   :: Word32,
  cpu_r13_und   :: Word32,
  cpu_r14       :: Word32,
  cpu_r14_abt   :: Word32,
  cpu_r14_fiq   :: Word32,
  cpu_r14_irq   :: Word32,
  cpu_r14_svc   :: Word32,
  cpu_r14_und   :: Word32,
  cpu_r15       :: Word32,
  cpu_cpsr      :: Word32,
  cpu_spsr_abt  :: Word32,
  cpu_spsr_fiq  :: Word32,
  cpu_spsr_irq  :: Word32,
  cpu_spsr_svc  :: Word32,
  cpu_spsr_und  :: Word32,
  cpu_memory    :: Memory,
  cpu_fetch     :: Maybe Word32,
  cpu_decode    :: Maybe Instruction
} deriving (Eq)

-- Instruction types.

data Instruction = Instruction Flag RawInstruction
  deriving (Show, Eq)

data RawInstruction = ADC Bool Register Register Shifter
                    | ADD Bool Register Register Shifter
                    | AND Bool Register Register Shifter
                    | B Bool (Immediate Word32)
                    | BIC Bool Register Register Shifter
                    | BX Register
                    | CMN Register Shifter
                    | CMP Register Shifter
                    | EOR Bool Register Register Shifter
                    | LDM1 AddressingMode4 [Register]
                    | LDM2 AddressingMode4 [Register]
                    | LDM3 AddressingMode4 [Register]
                    | LDR Register AddressingMode2
                    | LDRB Register AddressingMode2
                    | LDRBT Register AddressingMode2
                    | LDRH Register AddressingMode3
                    | LDRT Register AddressingMode2
                    | LDRSB Register AddressingMode3
                    | LDRSH Register AddressingMode3
                    | MLA Bool Register Register Register Register
                    | MOV Bool Register Shifter
                    | MRS Bool Register
                    | MSR_immediate Bool (Immediate Word32) (Immediate Word32) (Immediate Int)
                    | MSR_register Bool (Immediate Word32) Register
                    | MUL Bool Register Register Register
                    | MVN Bool Register Shifter
                    | NOP
                    | ORR Bool Register Register Shifter
                    | RSB Bool Register Register Shifter
                    | RSC Bool Register Register Shifter
                    | SBC Bool Register Register Shifter
                    | SMLAL Bool Register Register Register Register
                    | SMULL Bool Register Register Register Register
                    | STM1 AddressingMode4 [Register]
                    | STM2 AddressingMode4 [Register]
                    | STR Register AddressingMode2
                    | STRB Register AddressingMode2
                    | STRBT Register AddressingMode2
                    | STRH Register AddressingMode3
                    | STRT Register AddressingMode2
                    | SUB Bool Register Register Shifter
                    | TEQ Register Shifter
                    | TST Register Shifter
                    | UMLAL Bool Register Register Register Register
                    | UMULL Bool Register Register Register Register
  deriving (Show, Eq)

-- Addressing mode types
data Shifter = I_operand (Immediate Word32) (Immediate Int)
             | R_operand Register
             | LSL_I_operand Register (Immediate Int)
             | LSL_R_operand Register Register
             | LSR_I_operand Register (Immediate Int)
             | LSR_R_operand Register Register
             | ASR_I_operand Register (Immediate Int)
             | ASR_R_operand Register Register
             | ROR_I_operand Register (Immediate Int)
             | ROR_R_operand Register Register
             | RRX_operand Register
  deriving (Show, Eq)

data AddressingModeType = NoIndex | PreIndex | PostIndex
  deriving (Show, Eq)

data AddressingMode2 = AddrMode2_1 AddressingModeType Bool Register (Immediate Word32)
                     | AddrMode2_2 AddressingModeType Bool Register Register
                     | AddrMode2_3 AddressingModeType Bool Register Register (Immediate Int) Int
  deriving (Show, Eq)

data AddressingMode3 = AddrMode3_1 AddressingModeType Bool Register (Immediate Word32) (Immediate Word32)
                     | AddrMode3_2 AddressingModeType Bool Register Register
  deriving (Show, Eq)

data AddressingMode4 = IA Bool Register
                     | IB Bool Register
                     | DA Bool Register
                     | DB Bool Register
  deriving (Show, Eq)
