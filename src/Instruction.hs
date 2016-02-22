module Instruction where

import Prelude hiding (EQ, LT, GT, not, (&&), (||))
import Boolean as B
import Processor

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

cond :: ConditionCode -> ConditionCodeFlags -> Bool
cond EQ = z
cond NE = not z
cond CS = c
cond HS = cond CS
cond CC = not c
cond LO = cond CC
cond MI = n
cond PL = not n
cond VS = v
cond VC = not v
cond HI = c && not z
cond LS = not c && z
cond GE = n `eq` v
cond LT = n `neq` v
cond GT = not z && (n `eq` v)
cond LE = z || (n `neq` v)
cond AL = true

data Instruction = Instruction ConditionCode RawInstruction
data RawInstruction = ADC     |
                      ADD     |
                      AND     |
                      B       |
                      BIC     |
                      BL      |
                      BX      |
                      CDP     |
                      CMN     |
                      CMP     |
                      EOR     |
                      LDC     |
                      LDM1    |
                      LDM2    |
                      LDM3    |
                      LDR     |
                      LDRB    |
                      LDRBT   |
                      LDRH    |
                      LDRSB   |
                      LDRSH   |
                      LDRT    |
                      MCR     |
                      MLA     |
                      MOV     |
                      MRC     |
                      MRS     |
                      MSR     |
                      MUL     |
                      MVN     |
                      ORR     |
                      RSB     |
                      RSC     |
                      SBC     |
                      SMLAL   |
                      SMULL   |
                      STC     |
                      STM1    |
                      STM2    |
                      STR     |
                      STRB    |
                      STRBT   |
                      STRH    |
                      STRT    |
                      SUB     |
                      SWI     |
                      SWP     |
                      SWPB    |
                      TEQ     |
                      TST     |
                      UMLAL   |
                      UMULL
