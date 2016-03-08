{-# LANGUAGE BinaryLiterals #-}
module Decoder where

import Data.Bits
import Data.Word
import Bits
import CPU
import Instruction

decodeCond :: Word32 -> Flag
decodeCond 0x0 = eq
decodeCond 0x1 = ne
decodeCond 0x2 = hs
decodeCond 0x3 = lo
decodeCond 0x4 = mi
decodeCond 0x5 = pl
decodeCond 0x6 = vs
decodeCond 0x7 = vc
decodeCond 0x8 = hi
decodeCond 0x9 = ls
decodeCond 0xA = ge
decodeCond 0xB = lt
decodeCond 0xC = gt
decodeCond 0xD = le
decodeCond 0xE = al
decodeCond 0xF = error "Decoded undefined condition code."

decodeInstruction :: Word32 -> Maybe Instruction
decodeInstruction x
  | (x .&. 0x0E000000) == 0x0A000000 = d decodeB
  | (x .&. 0x0FFFFFF0) == 0x012FFF10 = d decodeBX
  | (x .&. 0x0DE00000) == 0x00000000 = d decodeAND
  | (x .&. 0x0DE00000) == 0x00200000 = d decodeEOR
  | (x .&. 0x0DE00000) == 0x00400000 = d decodeSUB
  | (x .&. 0x0DE00000) == 0x00600000 = d decodeRSB
  | (x .&. 0x0DE00000) == 0x00800000 = d decodeADD
  | (x .&. 0x0DE00000) == 0x00A00000 = d decodeADC
  | (x .&. 0x0DE00000) == 0x00C00000 = d decodeSBC
  | (x .&. 0x0DE00000) == 0x00E00000 = d decodeRSC
  | (x .&. 0x0DE00000) == 0x01000000 = d decodeTST
  | (x .&. 0x0DE00000) == 0x01200000 = d decodeTEQ
  | (x .&. 0x0DE00000) == 0x01400000 = d decodeCMP
  | (x .&. 0x0DE00000) == 0x01600000 = d decodeCMN
  | (x .&. 0x0DE00000) == 0x01800000 = d decodeORR
  | (x .&. 0x0DE00000) == 0x01A00000 = d decodeMOV
  | (x .&. 0x0DE00000) == 0x01C00000 = d decodeBIC
  | (x .&. 0x0DE00000) == 0x01E00000 = d decodeMVN
  | otherwise = Nothing
  where condition = decodeCond (x `shiftR` 0x1C)
        d :: (Word32 -> Maybe RawInstruction) -> Maybe Instruction
        d f = case (f x) of
                (Just ri) -> (Just $ Instruction condition ri)
                Nothing   -> Nothing

-- Branch instructions.
decodeB x = Just $ B (x `testBit` 24) (pure $ x .&. 0x00FFFFFF)
decodeBX x = Just $ BX (register (fromIntegral $ x .&. 0x0000000F))
-- Data processing instructions.
decodeDataProcess f x = Just $ f sFlag rd rn shift
  where sFlag = x `testBit` 20
        rd = register (fromIntegral $ bitRange 12 15 x)
        rn = register (fromIntegral $ bitRange 16 19 x)
        shift = decodeShifterOperand x
decodeAND = decodeDataProcess AND
decodeEOR = decodeDataProcess EOR
decodeSUB = decodeDataProcess SUB
decodeRSB = decodeDataProcess RSB
decodeADD = decodeDataProcess ADD
decodeADC = decodeDataProcess ADC
decodeSBC = decodeDataProcess SBC
decodeRSC = decodeDataProcess RSC
decodeTST x = Just $ TST rn shift
  where rn = register (fromIntegral $ bitRange 16 19 x)
        shift = decodeShifterOperand x
decodeTEQ x = Just $ TST rn shift
  where rn = register (fromIntegral $ bitRange 16 19 x)
        shift = decodeShifterOperand x
decodeCMP x = Just $ CMP rn shift
  where rn = register (fromIntegral $ bitRange 16 19 x)
        shift = decodeShifterOperand x
decodeCMN x = Just $ CMN rn shift
  where rn = register (fromIntegral $ bitRange 16 19 x)
        shift = decodeShifterOperand x
decodeORR = decodeDataProcess ORR
decodeMOV x = Just $ MOV sFlag rd shift
  where sFlag = x `testBit` 20
        rd = register (fromIntegral $ bitRange 12 15 x)
        shift = decodeShifterOperand x
decodeBIC = decodeDataProcess BIC
decodeMVN x = Just $ MVN sFlag rd shift
  where sFlag = x `testBit` 20
        rd = register (fromIntegral $ bitRange 12 15 x)
        shift = decodeShifterOperand x

-- 32-bit immediate
decodeShifterOperand x
  | x .&. 0x0E000000 == 0x02000000 = I_operand immed8 rotateImm
  where rotateImm = pure $ (fromIntegral $ bitRange 8 11 x)
        immed8 = pure $ bitRange 0 7 x
-- Immediate shifts
decodeShifterOperand x
  | (not bit4) && (shift == 0) && (shiftImm == 0) = R_operand rm
  | (not bit4) && (shift == 0) = LSL_I_operand rm shiftImm
  | (bit4) && (shift == 0) = LSL_R_operand rm rs
  | (not bit4) && (shift == 1) = LSR_I_operand rm shiftImm
  | (bit4) && (shift == 1) = LSR_R_operand rm rs
  | (not bit4) && (shift == 2) = ASR_I_operand rm shiftImm
  | (bit4) && (shift == 2) = ASR_R_operand rm rs
  | (not bit4) && (shift == 3) = ROR_I_operand rm shiftImm
  | (bit4) && (shift == 3) = ROR_R_operand rm rs
  where rm = register (fromIntegral $ bitRange 0 3 x) 
        rs = register (fromIntegral $ bitRange 8 11 x)
        shiftImm = pure $ (fromIntegral $ bitRange 7 11 x)
        shift = bitRange 5 6 x
        bit4 = x `testBit` 4
