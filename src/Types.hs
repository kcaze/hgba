module Types where

import qualified Data.Map.Strict as Map
import Prelude hiding (EQ, LT, GT)
import Data.Word
import Data.Bits

type Byte = Word8
type Address = Word32
type Register = Int
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
                      | AND Bool Register Register AddressMode1
                      | B Bool Word32
                      | BIC
                      | BX Register
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
