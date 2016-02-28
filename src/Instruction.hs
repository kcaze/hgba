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
cpu `executeRawInstruction` (AND sbit rn rd addressMode) =
  where (operand, carry_out) = evalAddressMode1 addressMode
        
-- Data processing instructions.

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
          | rs' < 32 = rm' `logicalShiftL` (fromIntegral rs')
          | otherwise = 0
        carry_out
          | rs' == 0 = getCFlag cpu
          | rs' < 32 = testBit rm' (32 - fromIntegral rs')
          | rs' == 32 = testBit rm' 0
          | otherwise = False
evalAddressMode1 (AddressMode1_5 rm shift_imm) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        operand
          | shift_imm == 0 = 0
          | otherwise = rm' `logicalShiftR` (fromIntegral shift_imm)
        carry_out
          | shift_imm == 0 = testBit rm' 31
          | otherwise = testBit rm' (fromIntegral shift_imm - 1)
evalAddressMode1 (AddressMode1_6 rm rs) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        rs' = (getRegister (fromIntegral rs) cpu) .&. 0x000000FF
        operand
          | rs' == 0 = rm'
          | rs' < 32 = rm' `logicalShiftR` (fromIntegral rs')
          | otherwise = 0
        carry_out
          | rs' == 0 = getCFlag cpu
          | rs' < 32 = testBit rm' (fromIntegral rs' - 1)
          | rs' == 32 = testBit rm' 31
          | otherwise = False
evalAddressMode1 (AddressMode1_7 rm shift_imm) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        operand
          | shift_imm == 0 && not (testBit rm' 31) = 0
          | shift_imm == 0 && testBit rm' 31 = 0xFFFFFFFF
          | otherwise = rm' `arithmeticShiftR` (fromIntegral shift_imm)
        carry_out
          | shift_imm == 0 = testBit rm' 31
          | otherwise = testBit rm' (fromIntegral shift_imm - 1)
evalAddressMode1 (AddressMode1_8 rm rs) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        rs' = (getRegister (fromIntegral rs) cpu) .&. 0x000000FF
        operand
          | rs' == 0 = rm'
          | rs' < 32 = rm' `arithmeticShiftR` (fromIntegral rs')
          | not (testBit rm 31) = 0
          | otherwise = 0xFFFFFFFF
        carry_out
          | rs' == 0 = getCFlag cpu
          | rs' < 32 = testBit rm' (fromIntegral rs' - 1)
          | otherwise = testBit rm' 31
evalAddressMode (AddressMode1_9 rm shift_imm) cpu
  | shift_imm == 0 = evalAddressMode (AddressMode1_11 rm) cpu
  | otherwise = (operand, carry_out)
    where rm' = getRegister (fromIntegral rm) cpu
          operand = rm' `rotateR` (fromIntegral shift_imm)
          carry_out = testBit rm' (fromIntegral shift_imm - 1)
evalAddressMode (AddressMode1_10 rm rs) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        rs' = (getRegister (fromIntegral rs) cpu) .&. 0x000000FF
        rs'' = (getRegister (fromIntegral rs) cpu) .&. 0x0000001F
        operand
          | rs' == 0 = rm'
          | rs'' == 0 = rm'
          | otherwise = rm' `rotateR` (fromIntegral rs'')
        carry_out
          | rs' == 0 = getCFlag cpu
          | rs'' == 0 = testBit rm' 31
          | otherwise = testBit rm' (fromIntegral rs'' - 1)
evalAddressMode (AddressMode1_11 rm) cpu = (operand, carry_out)
  where rm' = getRegister (fromIntegral rm) cpu
        cbit = if getCFlag cpu then 1 else 0
        operand = (cbit `logicalShiftL` 31) .|. (rm' `logicalShiftR` 1)
        carry_out = testBit rm' 0
