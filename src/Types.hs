module Types where

import qualified Data.Map.Strict as Map
import Prelude hiding (EQ, LT, GT)
import Data.Word
import Data.Bits

type Byte = Word8
type Address = Word32
type Register = Word32
type AddressSpace = Map.Map Address Byte

data Instruction = Instruction ConditionCode RawInstruction
  deriving (Eq, Show)

data ConditionCode = EQ | 
                     NE |
                     CS |
                     HS |
                     CC |
                     LO |
                     MI |
                     PL |
                     VS |
                     VC |
                     HI |
                     LS |
                     GE |
                     LT |
                     GT |
                     LE |
                     AL
  deriving (Eq, Show)

data RawInstruction =   ADC
                      | ADD
                      | AND
                      | B Bool Word32
                      | BIC
                      | BX
                      | CDP
                      | CMN
                      | CMP
                      | EOR
                      | LDC
                      | LDM1
                      | LDM2
                      | LDM3
                      | LDR
                      | LDRB
                      | LDRBT
                      | LDRH
                      | LDRSB
                      | LDRSH
                      | LDRT
                      | MCR
                      | MLA
                      | MOV
                      | MRC
                      | MRS
                      | MSR
                      | MUL
                      | MVN
                      | ORR
                      | RSB
                      | RSC
                      | SBC
                      | SMLAL
                      | SMULL
                      | STC
                      | STM1
                      | STM2
                      | STR
                      | STRB
                      | STRBT
                      | STRH
                      | STRT
                      | SUB
                      | SWI
                      | SWP
                      | SWPB
                      | TEQ
                      | TST
                      | UMLAL
                      | UMULL
  deriving (Eq, Show)

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
  } deriving (Eq, Show)

data StatusRegisters = StatusRegisters
  {
    conditionCodeFlags :: ConditionCodeFlags,
    irqInterruptMask :: Bool,
    fiqInterruptMask  :: Bool,
    thumbStateFlag :: Bool,
    processorMode :: ProcessorMode
  } deriving (Eq, Show)

data ConditionCodeFlags = ConditionCodeFlags
  {
    n :: Bool,
    z :: Bool,
    c :: Bool,
    v :: Bool
  } deriving (Eq, Show)

data ProcessorMode = User       |
                     FIQ        |
                     IRQ        |
                     Supervisor |
                     Abort      |
                     Undefined  |
                     System
  deriving (Eq, Show)

data Pipeline = Pipeline
  {
    decode :: Maybe Word32,
    execute :: Maybe Instruction
  } deriving (Eq, Show)

data CPU = CPU
  {
    registers :: GeneralPurposeRegisters,
    cpsr :: StatusRegisters,
    spsr_abt :: StatusRegisters,
    spsr_fiq :: StatusRegisters,
    spsr_irq :: StatusRegisters,
    spsr_svc :: StatusRegisters,
    spsr_und :: StatusRegisters,
    pipeline :: Pipeline,
    memory :: AddressSpace
  } deriving Show
