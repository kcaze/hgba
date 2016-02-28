module Types where

import qualified Data.Map.Strict as Map
import Prelude hiding (EQ, LT, GT)
import Control.Applicative
import Data.Word
import Data.Bits

type Byte = Word8
type Address = Word32
type Register = Int
type AddressSpace = Map.Map Address Byte

type Get a = CPU -> a
type Set a = CPU -> a -> CPU

data Instruction = Instruction ConditionCode RawInstruction
  deriving (Eq, Show)

(/+/) :: (Num a) => Get a -> Get a -> Get a
(/+/) = liftA2 (+)

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

data RawInstruction =   ADC Bool Register Register AddressMode1
                      | ADD Bool Register Register AddressMode1
                      | AND Bool Register Register AddressMode1
                      | B Bool Word32
                      | BIC Bool Register Register AddressMode1
                      | BX Register
                      | CDP
                      | CMN Register AddressMode1
                      | CMP Register AddressMode1
                      | EOR Bool Register Register AddressMode1
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
                      | MLA Bool Register Register Register Register
                      | MOV Bool Register AddressMode1
                      | MRC
                      | MRS Bool Register
                      | MSR Bool Word32 AddressMode1
                      | MUL Bool Register Register Register
                      | MVN Bool Register AddressMode1
                      | ORR Bool Register Register AddressMode1
                      | RSB Bool Register Register AddressMode1
                      | RSC Bool Register Register AddressMode1
                      | SBC Bool Register Register AddressMode1
                      | SMLAL Bool Register Register Register Register
                      | SMULL Bool Register Register Register Register
                      | STC
                      | STM1
                      | STM2
                      | STR
                      | STRB
                      | STRBT
                      | STRH
                      | STRT
                      | SUB Bool Register Register AddressMode1
                      | SWI
                      | SWP
                      | SWPB
                      | TEQ Register AddressMode1
                      | TST Register AddressMode1
                      | UMLAL Bool Register Register Register Register
                      | UMULL Bool Register Register Register Register
  deriving (Eq, Show)

data AddressMode1 =   AddressMode1_1 Word32 Word32 -- #<rotate_imm> #<immed_8>
                    | AddressMode1_2 Register -- <Rm>
                    | AddressMode1_3 Register Word32 -- <Rm>, LSL #<shift_imm>
                    | AddressMode1_4 Register Register -- <Rm>, LSL <Rs>
                    | AddressMode1_5 Register Word32 -- <Rm>, LSR <#shift_imm>
                    | AddressMode1_6 Register Register -- <Rm>, LSR <Rs>
                    | AddressMode1_7 Register Word32 -- <Rm>, ASR <#shift_imm>
                    | AddressMode1_8 Register Register -- <Rm>, ASR <Rs>
                    | AddressMode1_9 Register Word32 -- <Rm>, ROR <#shift_imm>
                    | AddressMode1_10 Register Register -- <Rm>, ROR <Rs>
                    | AddressMode1_11 Register -- <Rm>, RRX
  deriving (Eq, Show)

data GeneralPurposeRegisters = GeneralPurposeRegisters
  {
    -- Unbanked registers.
    r0 :: Word32,
    r1 :: Word32,
    r2 :: Word32,
    r3 :: Word32,
    r4 :: Word32,
    r5 :: Word32,
    r6 :: Word32,
    r7 :: Word32,

    -- Banked registers.
    r8        :: Word32,
    r8_fiq    :: Word32,
    r9        :: Word32,
    r9_fiq    :: Word32,
    r10       :: Word32,
    r10_fiq   :: Word32,
    r11       :: Word32,
    r11_fiq   :: Word32,
    r12       :: Word32,
    r12_fiq   :: Word32,
    r13       :: Word32,
    r13_svc   :: Word32,
    r13_abt   :: Word32,
    r13_und   :: Word32,
    r13_irq   :: Word32,
    r13_fiq   :: Word32,
    r14       :: Word32,
    r14_svc   :: Word32,
    r14_abt   :: Word32,
    r14_und   :: Word32,
    r14_irq   :: Word32,
    r14_fiq   :: Word32,

    -- Program counter.
    pc :: Word32
  } deriving (Eq, Show)

data StatusRegister = StatusRegister
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
    cpsr :: StatusRegister,
    spsr_abt :: StatusRegister,
    spsr_fiq :: StatusRegister,
    spsr_irq :: StatusRegister,
    spsr_svc :: StatusRegister,
    spsr_und :: StatusRegister,
    pipeline :: Pipeline,
    memory :: AddressSpace
  } deriving Show
