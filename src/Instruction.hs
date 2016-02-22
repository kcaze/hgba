module Instruction where

import Data.Bits
import Data.Word
import Prelude hiding (EQ, LT, GT, not, (&&), (||))

import Boolean
import Util
import Types
import Register

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

instructionSize :: CPU -> Word32
instructionSize cpu
  | thumb = 4
  | otherwise = 8
  where thumb = thumbStateFlag . cpsr $ cpu

executeInstruction :: CPU -> Instruction -> CPU
cpu `executeInstruction` (Instruction condition rawInstruction)
  | cond condition (getConditionCodeFlags cpu) = cpu `executeRawInstruction` rawInstruction
  | otherwise = cpu

executeRawInstruction :: CPU -> RawInstruction -> CPU
-- Branch instructions.
cpu `executeRawInstruction` (B link target_address)
  | link = cpu `setR14` lr' `setR15` pc'
  | otherwise = cpu `setR15` pc'
  where target_address' = shiftL (signExtend 24 30 target_address) 2
        pc' = getR15 cpu + target_address'
        lr' = getR15 cpu - (if (thumb cpu) then 4 else 8)
