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
  | getThumbStateFlag cpu = 4
  | otherwise = 8

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
        lr' = getR15 cpu - (if (getThumbStateFlag cpu) then 4 else 8)
cpu `executeRawInstruction` (BX rm) = cpu `setThumbStateFlag` thumbStateFlag' `setR15` pc'
  where thumbStateFlag' = testBit rm 0
        pc' = (getRegister (fromIntegral rm) cpu) .&. 0xFFFFFFFE

-- Returns (shifter_operand, shifter_carry_out)
evalAddressMode1 :: AddressMode1 -> CPU -> (Word32, Bool)
evalAddressMode1 (AddressMode1_1 rotate_imm immed_8) cpu = (operand, carry_out)
  where operand = immed_8 `rotateR` (fromIntegral rotate_imm * 2)
        carry_out
          | rotate_imm == 0 = getCFlag cpu
          | otherwise = testBit operand 31
evalAddressMode1 (AddressMode1_2 rm) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        operand = rm'
        carry_out = getCFlag cpu
evalAddressMode1 (AddressMode1_3 rm shift_imm) cpu = (operand, carry_out) 
  where rm' = getRegister (fromIntegral rm) cpu
        operand
          | shift_imm == 0 = rm'
          | otherwise = rm' `logicalShiftL` (fromIntegral shift_imm)
        carry_out
          | shift_imm == 0 = getCFlag cpu
          | otherwise = testBit rm' (fromIntegral $ 32 - shift_imm)
evalAddressMode1 (AddressMode1_4 rm rs) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        rs' = (getRegister (fromIntegral rs) cpu) .&. 0x000000FF
        operand
          | rs' == 0 = rm'
          | rs' < 32 = rm' `logicalShiftL` rs'
          | otherwise = 0
        carry_out
          | rs' == 0 = getCFlag cpu
          | rs' < 32 = testBit rm' (32 - rs')
          | rs' == 32 = testBit rm' 0
          | otherwise = 0
evalAddressMode1 (AddressMode1_5 rm shift_imm) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        operand
          | shift_imm == 0 = 0
          | otherwise = rm' `logicalShiftR` shift_imm -- BE CAREFUL: SHIFT BY 32 IS ENCODED BY SHIFT_IMM == 0
--evalAddress (AddressMode1_4 rm rs) =
--  (getRegister (fromIntegral rm) cpu) `logicalShiftL` shift_imm
