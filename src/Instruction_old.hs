module Instruction where

import Data.Bits
import Data.Word
import Data.Int
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
cpu `executeRawInstruction` (BX rm) = cpu'
  where thumbStateFlag' = testBit rm 0
        pc' = (cpu `getRegister` rm) .&. 0xFFFFFFFE
        cpu' = cpu `setThumbStateFlag` thumbStateFlag' `setR15` pc'
-- Data processing instructions.
cpu `executeRawInstruction` (AND sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rd' = (cpu `getRegister` rn) .&. shifter_operand
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (shifter_carry_out)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (EOR sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rd' = (cpu `getRegister` rn) `xor` shifter_operand
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (shifter_carry_out)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (SUB sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rn' = (cpu `getRegister` rn)
        rd' = rn' - shifter_operand
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (not $ borrowFrom rn' shifter_operand)
                                 `setVFlag` (overflowFromSub rn' shifter_operand)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (RSB sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rn' = (cpu `getRegister` rn)
        rd' = shifter_operand - rn'
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (not $ borrowFrom shifter_operand rn')
                                 `setVFlag` (overflowFromSub shifter_operand rn')
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (ADD sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rn' = (cpu `getRegister` rn)
        rd' = rn' + shifter_operand
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (carryFrom rn' shifter_operand)
                                 `setVFlag` (overflowFromAdd rn' shifter_operand)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (ADC sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        cFlag = if getCFlag cpu then 1 else 0
        rn' = (cpu `getRegister` rn)
        rd' = rn' + shifter_operand + cFlag
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (carryFrom rn' (shifter_operand + cFlag))
                                 `setVFlag` (overflowFromAdd rn' (shifter_operand + cFlag))
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (SBC sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        notCFlag = if getCFlag cpu then 0 else 1
        rn' = (cpu `getRegister` rn)
        rd' = rn' - (shifter_operand + notCFlag)
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (not $ borrowFrom rn' (shifter_operand + notCFlag))
                                 `setVFlag` (overflowFromSub rn' (shifter_operand + notCFlag))
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (RSC sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        notCFlag = if getCFlag cpu then 0 else 1
        rn' = (cpu `getRegister` rn)
        rd' = shifter_operand - (rn' + notCFlag)
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (not $ borrowFrom shifter_operand (rn' + notCFlag))
                                 `setVFlag` (overflowFromSub shifter_operand (rn' + notCFlag))
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (TST rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        alu_out = (cpu `getRegister` rn) .&. shifter_operand
        cpu' = cpu `setNFlag` (testBit alu_out 31)
                   `setZFlag` (alu_out == 0)
                   `setCFlag` (shifter_carry_out)
cpu `executeRawInstruction` (TEQ rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        alu_out = (cpu `getRegister` rn) `xor` shifter_operand
        cpu' = cpu `setNFlag` (testBit alu_out 31)
                   `setZFlag` (alu_out == 0)
                   `setCFlag` (shifter_carry_out)
cpu `executeRawInstruction` (CMP rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rn' = cpu `getRegister` rn
        alu_out = rn' - shifter_operand
        cpu' = cpu `setNFlag` (testBit alu_out 31)
                   `setZFlag` (alu_out == 0)
                   `setCFlag` (not $ borrowFrom rn' shifter_operand)
                   `setVFlag` (overflowFromSub rn' shifter_operand)
cpu `executeRawInstruction` (CMN rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rn' = cpu `getRegister` rn
        alu_out = rn' + shifter_operand
        cpu' = cpu `setNFlag` (testBit alu_out 31)
                   `setZFlag` (alu_out == 0)
                   `setCFlag` (carryFrom rn' shifter_operand)
                   `setVFlag` (overflowFromAdd rn' shifter_operand)
cpu `executeRawInstruction` (ORR sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rd' = (cpu `getRegister` rn) .|. shifter_operand
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (shifter_carry_out)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (MOV sbit rd addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rd' = shifter_operand
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (shifter_carry_out)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (BIC sbit rd rn addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rd' = (cpu `getRegister` rn) .&. (complement shifter_operand)
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (shifter_carry_out)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (MVN sbit rd addressMode) = cpu'
  where (shifter_operand, shifter_carry_out) = evalAddressMode1 addressMode cpu
        rd' = complement shifter_operand
        cpsr'
          | sbit && rd == 15 = getSPSR cpu
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
                                 `setCFlag` (shifter_carry_out)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
-- Multiply instructions.
cpu `executeRawInstruction` (MUL sbit rd rm rs) = cpu'
  where rd' = (cpu `getRegister` rm) * (cpu `getRegister` rs)
        cpsr'
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (MLA sbit rd rm rs rn) = cpu'
  where rd' = (cpu `getRegister` rm) * (cpu `getRegister` rs) + (cpu `getRegister` rn)
        cpsr'
          | sbit = getCPSR $ cpu `setNFlag` (testBit rd' 31)
                                 `setZFlag` (rd' == 0)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rd rd'
               `setCPSR` cpsr'
cpu `executeRawInstruction` (SMULL sbit rdlo rdhi rm rs) = cpu''
  where rm' = fromIntegral (cpu `getRegister` rm) :: Int64
        rs' = fromIntegral (cpu `getRegister` rs) :: Int64
        rdhi' = fromIntegral $ (rm' * rs') `shiftR` 32 :: Word32
        rdlo' = fromIntegral $ (rm' * rs') .&. 0xFFFFFFFF :: Word32
        cpsr'
          | sbit = getCPSR $ cpu `setNFlag` (testBit rdhi' 31)
                                 `setZFlag` (rdhi' == 0 && rdlo' == 0)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rdhi rdhi'
        cpu'' = setRegister cpu' rdlo rdlo'
                `setCPSR` cpsr'
cpu `executeRawInstruction` (UMULL sbit rdlo rdhi rm rs) = cpu''
  where rm' = fromIntegral (cpu `getRegister` rm) :: Word64
        rs' = fromIntegral (cpu `getRegister` rs) :: Word64
        rdhi' = fromIntegral $ (rm' * rs') `shiftR` 32 :: Word32
        rdlo' = fromIntegral $ (rm' * rs') .&. 0xFFFFFFFF :: Word32
        cpsr'
          | sbit = getCPSR $ cpu `setNFlag` (testBit rdhi' 31)
                                 `setZFlag` (rdhi' == 0 && rdlo' == 0)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rdhi rdhi'
        cpu'' = setRegister cpu' rdlo rdlo'
                `setCPSR` cpsr'
cpu `executeRawInstruction` (SMLAL sbit rdlo rdhi rm rs) = cpu''
  where rm' = fromIntegral (cpu `getRegister` rm) :: Int64
        rs' = fromIntegral (cpu `getRegister` rs) :: Int64
        rdlo' = fromIntegral $ (rm' * rs') .&. 0xFFFFFFFF :: Word32
        rdlo'' = rdlo' + (cpu `getRegister` rdlo)
        rdhi' = fromIntegral $ (rm' * rs') `shiftR` 32 :: Word32
        rdhi'' = rdhi' + (cpu `getRegister` rdhi)
                       + (if carryFrom rdlo' (cpu `getRegister` rdlo)
                          then 1
                          else 0)
        cpsr'
          | sbit = getCPSR $ cpu `setNFlag` (testBit rdhi' 31)
                                 `setZFlag` (rdhi' == 0 && rdlo' == 0)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rdhi rdhi''
        cpu'' = setRegister cpu' rdlo rdlo''
                `setCPSR` cpsr'
cpu `executeRawInstruction` (UMLAL sbit rdlo rdhi rm rs) = cpu''
  where rm' = fromIntegral (cpu `getRegister` rm) :: Word64
        rs' = fromIntegral (cpu `getRegister` rs) :: Word64
        rdlo' = fromIntegral $ (rm' * rs') .&. 0xFFFFFFFF :: Word32
        rdlo'' = rdlo' + (cpu `getRegister` rdlo)
        rdhi' = fromIntegral $ (rm' * rs') `shiftR` 32 :: Word32
        rdhi'' = rdhi' + (cpu `getRegister` rdhi)
                       + (if carryFrom rdlo' (cpu `getRegister` rdlo)
                          then 1
                          else 0)
        cpsr'
          | sbit = getCPSR $ cpu `setNFlag` (testBit rdhi' 31)
                                 `setZFlag` (rdhi' == 0 && rdlo' == 0)
          | otherwise = getCPSR cpu
        cpu' = setRegister cpu rdhi rdhi''
        cpu'' = setRegister cpu' rdlo rdlo''
                `setCPSR` cpsr'

-- Status register access instructions
cpu `executeRawInstruction` (MRS rbit rd) = cpu'
  where rd'
          | rbit = statusRegisterToWord32 . getSPSR $ cpu
          | otherwise = statusRegisterToWord32 . getCPSR $ cpu
        cpu' = setRegister cpu rd rd'
cpu `executeRawInstruction` (MSR1 rbit field_mask addressMode) = cpu'
  where (operand, _)= evalAddressMode1 addressMode
        byte_mask = (if (testBit field_mask 0) then 0x000000FF else 0) .|.
                    (if (testBit field_mask 1) then 0x0000FF00 else 0) .|.
                    (if (testBit field_mask 2) then 0x00FF0000 else 0) .|.
                    (if (testBit field_mask 3) then 0xFF000000 else 0)
        userMask  = 0xF0000000
        privMask  = 0x0000000F
        stateMask = 0x00000020
        inAPrivilegedMode = getProcessorMode cpu /= User
        mask
          | not rbit && inAPrivilegedMode = byte_mask .&. (userMask .|. privMask)
          | not rbit = byte_mask .&. userMask
          | otherwise = byte_mask .&. (userMask .|. privMask .|. stateMask)
        cpsrWord32 = statusRegisterToWord32 . getCPSR $ cpu
        cpsr' = (cpsrWord32 .&. complement mask)
        cpu'
          | cpu `setCPSR` setRegister cpu rd rd'
        

-- Addressing Mode 1 - Data-processing operands
-- Returns (shifter_operand, shifter_carry_out)
evalAddressMode1 :: AddressMode1 -> CPU -> (Word32, Bool)
evalAddressMode1 (AddressMode1_1 rotate_imm immed_8) cpu = (operand, carry_out)
  where operand = immed_8 `rotateR` (fromIntegral rotate_imm * 2)
        carry_out
          | rotate_imm == 0 = getCFlag cpu
          | otherwise = testBit operand 31
evalAddressMode1 (AddressMode1_2 rm) cpu = (operand, carry_out)
  where rm' = cpu `getRegister` rm
        operand = rm'
        carry_out = getCFlag cpu
evalAddressMode1 (AddressMode1_3 rm shift_imm) cpu = (operand, carry_out) 
  where rm' = cpu `getRegister` rm
        operand
          | shift_imm == 0 = rm'
          | otherwise = rm' `logicalShiftL` (fromIntegral shift_imm)
        carry_out
          | shift_imm == 0 = getCFlag cpu
          | otherwise = testBit rm' (fromIntegral $ 32 - shift_imm)
evalAddressMode1 (AddressMode1_4 rm rs) cpu = (operand, carry_out)
  where rm' = cpu `getRegister` rm
        rs' = (cpu `getRegister` rs) .&. 0x000000FF
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
  where rm' = cpu `getRegister` rm
        operand
          | shift_imm == 0 = 0
          | otherwise = rm' `logicalShiftR` (fromIntegral shift_imm)
        carry_out
          | shift_imm == 0 = testBit rm' 31
          | otherwise = testBit rm' (fromIntegral shift_imm - 1)
evalAddressMode1 (AddressMode1_6 rm rs) cpu = (operand, carry_out)
  where rm' = cpu `getRegister` rm
        rs' = (cpu `getRegister` rs) .&. 0x000000FF
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
  where rm' = cpu `getRegister` rm
        operand
          | shift_imm == 0 && not (testBit rm' 31) = 0
          | shift_imm == 0 && testBit rm' 31 = 0xFFFFFFFF
          | otherwise = rm' `arithmeticShiftR` (fromIntegral shift_imm)
        carry_out
          | shift_imm == 0 = testBit rm' 31
          | otherwise = testBit rm' (fromIntegral shift_imm - 1)
evalAddressMode1 (AddressMode1_8 rm rs) cpu = (operand, carry_out)
  where rm' = cpu `getRegister` rm
        rs' = (cpu `getRegister` rs) .&. 0x000000FF
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
    where rm' = cpu `getRegister` rm
          operand = rm' `rotateR` (fromIntegral shift_imm)
          carry_out = testBit rm' (fromIntegral shift_imm - 1)
evalAddressMode (AddressMode1_10 rm rs) cpu = (operand, carry_out)
  where rm' = cpu `getRegister` rm
        rs' = (cpu `getRegister` rs) .&. 0x000000FF
        rs'' = (cpu `getRegister` rs) .&. 0x0000001F
        operand
          | rs' == 0 = rm'
          | rs'' == 0 = rm'
          | otherwise = rm' `rotateR` (fromIntegral rs'')
        carry_out
          | rs' == 0 = getCFlag cpu
          | rs'' == 0 = testBit rm' 31
          | otherwise = testBit rm' (fromIntegral rs'' - 1)
evalAddressMode (AddressMode1_11 rm) cpu = (operand, carry_out)
  where rm' = cpu `getRegister` rm
        cbit = if getCFlag cpu then 1 else 0
        operand = (cbit `logicalShiftL` 31) .|. (rm' `logicalShiftR` 1)
        carry_out = testBit rm' 0
