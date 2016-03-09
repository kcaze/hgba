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
  | (x .&. 0x0DE00000) == 0x00000000 && isData x = d decodeAND
  | (x .&. 0x0DE00000) == 0x00200000 && isData x = d decodeEOR
  | (x .&. 0x0DE00000) == 0x00400000 && isData x = d decodeSUB
  | (x .&. 0x0DE00000) == 0x00600000 && isData x = d decodeRSB
  | (x .&. 0x0DE00000) == 0x00800000 && isData x = d decodeADD
  | (x .&. 0x0DE00000) == 0x00A00000 && isData x = d decodeADC
  | (x .&. 0x0DE00000) == 0x00C00000 && isData x = d decodeSBC
  | (x .&. 0x0DE00000) == 0x00E00000 && isData x = d decodeRSC
  | (x .&. 0x0DE0F000) == 0x01000000 && isData x = d decodeTST
  | (x .&. 0x0DE0F000) == 0x01200000 && isData x = d decodeTEQ
  | (x .&. 0x0DE0F000) == 0x01400000 && isData x = d decodeCMP
  | (x .&. 0x0DE0F000) == 0x01600000 && isData x = d decodeCMN
  | (x .&. 0x0DE00000) == 0x01800000 && isData x = d decodeORR
  | (x .&. 0x0DEF0000) == 0x01A00000 && isData x = d decodeMOV
  | (x .&. 0x0DE00000) == 0x01C00000 && isData x = d decodeBIC
  | (x .&. 0x0DEF0000) == 0x01E00000 && isData x = d decodeMVN
  | (x .&. 0x0FE000F0) == 0x00000090 = d decodeMUL
  | (x .&. 0x0FE000F0) == 0x00200090 = d decodeMLA
  | (x .&. 0x0FE000F0) == 0x00C00090 = d decodeSMULL
  | (x .&. 0x0FE000F0) == 0x00800090 = d decodeUMULL
  | (x .&. 0x0FE000F0) == 0x00E00090 = d decodeSMLAL
  | (x .&. 0x0FE000F0) == 0x00A00090 = d decodeUMLAL
  | (x .&. 0x0FBF0FFF) == 0x010F0000 = d decodeMRS
  | (x .&. 0x0FB0F000) == 0x0320F000 = d decodeMSR_immediate
  | (x .&. 0x0FB0FFF0) == 0x0120F000 = d decodeMSR_register
  | (x .&. 0x0C500000) == 0x04100000 = d decodeLDR
  | (x .&. 0x0C500000) == 0x04500000 = d decodeLDRB
  | (x .&. 0x0D700000) == 0x04700000 = d decodeLDRBT -- TODO
  | (x .&. 0x0E1000F0) == 0x001000B0 = d decodeLDRH
  | (x .&. 0x0D700000) == 0x04300000 = d decodeLDRT -- TODO
  | (x .&. 0x0E1000F0) == 0x001000D0 = d decodeLDRSB
  | (x .&. 0x0E1000F0) == 0x001000F0 = d decodeLDRSH
  | (x .&. 0x0C500000) == 0x04000000 = d decodeSTR
  | (x .&. 0x0D700000) == 0x04200000 = d decodeSTRT -- TODO
  | (x .&. 0x0C500000) == 0x04400000 = d decodeSTRB
  | (x .&. 0x0D700000) == 0x04600000 = d decodeSTRBT -- TODO
  | (x .&. 0x0E1000F0) == 0x000000B0 = d decodeSTRH
  | otherwise = Nothing
  where condition = decodeCond (x `shiftR` 0x1C)
        d :: (Word32 -> Maybe RawInstruction) -> Maybe Instruction
        d f = case (f x) of
                (Just ri) -> (Just $ Instruction condition ri)
                Nothing   -> Nothing
        isData x = (x .&. 0x02000090) /= 0x00000090

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
decodeTEQ x = Just $ TEQ rn shift
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
-- Multiply instructions
decodeMUL x = Just $ MUL sFlag rd rm rs
  where sFlag = x `testBit` 20
        rd = register (fromIntegral $ bitRange 16 19 x)
        rs = register (fromIntegral $ bitRange 8 11 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
decodeMLA x = Just $ MLA sFlag rd rm rs rn
  where sFlag = x `testBit` 20
        rd = register (fromIntegral $ bitRange 16 19 x)
        rn = register (fromIntegral $ bitRange 12 15 x)
        rs = register (fromIntegral $ bitRange 8 11 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
decodeSMULL x = Just $ SMULL sFlag rdlo rdhi rm rs
  where sFlag = x `testBit` 20
        rdhi = register (fromIntegral $ bitRange 16 19 x)
        rdlo = register (fromIntegral $ bitRange 12 15 x)
        rs = register (fromIntegral $ bitRange 8 11 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
decodeUMULL x = Just $ UMULL sFlag rdlo rdhi rm rs
  where sFlag = x `testBit` 20
        rdhi = register (fromIntegral $ bitRange 16 19 x)
        rdlo = register (fromIntegral $ bitRange 12 15 x)
        rs = register (fromIntegral $ bitRange 8 11 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
decodeSMLAL x = Just $ SMLAL sFlag rdlo rdhi rm rs
  where sFlag = x `testBit` 20
        rdhi = register (fromIntegral $ bitRange 16 19 x)
        rdlo = register (fromIntegral $ bitRange 12 15 x)
        rs = register (fromIntegral $ bitRange 8 11 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
decodeUMLAL x = Just $ UMLAL sFlag rdlo rdhi rm rs
  where sFlag = x `testBit` 20
        rdhi = register (fromIntegral $ bitRange 16 19 x)
        rdlo = register (fromIntegral $ bitRange 12 15 x)
        rs = register (fromIntegral $ bitRange 8 11 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
-- Status register access instructions.
decodeMRS x = Just $ MRS rFlag rd
  where rFlag = x `testBit` 22
        rd = register (fromIntegral $ bitRange 12 15 x)
decodeMSR_immediate x = Just $ MSR_immediate rFlag fieldMask immed8 rotateImm
  where rFlag = x `testBit` 22
        fieldMask = pure $ bitRange 16 19 x
        immed8 = pure $ bitRange 0 7 x
        rotateImm = pure $ (fromIntegral $ bitRange 8 11 x)
decodeMSR_register x = Just $ MSR_register rFlag fieldMask rm
  where rFlag = x `testBit` 22
        fieldMask = pure $ bitRange 16 19 x
        rm = register (fromIntegral $ bitRange 0 3 x)
-- Load and store instructions.
decodeLoadStore f x = Just $ f rd am
  where rd = register (fromIntegral $ bitRange 12 15 x)
        am = decodeAddressingMode2 x
decodeLoadStore' f x = Just $ f rd am
  where rd = register (fromIntegral $ bitRange 12 15 x)
        am = decodeAddressingMode3 x
decodeLDR = decodeLoadStore LDR
decodeLDRB = decodeLoadStore LDRB
decodeLDRBT = decodeLoadStore LDRBT
decodeLDRH = decodeLoadStore' LDRH
decodeLDRT = decodeLoadStore LDRT
decodeLDRSB = decodeLoadStore' LDRSB
decodeLDRSH = decodeLoadStore' LDRSH
decodeSTR = decodeLoadStore STR
decodeSTRB = decodeLoadStore STRB
decodeSTRBT = decodeLoadStore STRBT
decodeSTRH = decodeLoadStore' STRH
decodeSTRT = decodeLoadStore STRT

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

-- Immediate offset/index
decodeAddressingMode2 x
  | not (x `testBit` 25) = AddrMode2_1 addrType uFlag rn offset
  where pFlag = x `testBit` 24
        wFlag = x `testBit` 21
        uFlag = x `testBit` 23
        rn = register (fromIntegral $ bitRange 16 19 x)
        offset = pure $ bitRange 0 11 x
        addrType = if (pFlag && wFlag)
                   then PreIndex
                   else if (pFlag && not wFlag)
                   then NoIndex
                   else PostIndex
-- Register offset/index
decodeAddressingMode2 x
  | (x .&. 0x00000FF0 == 0) = AddrMode2_2 addrType uFlag rn rm
  where pFlag = x `testBit` 24
        wFlag = x `testBit` 21
        uFlag = x `testBit` 23
        rn = register (fromIntegral $ bitRange 16 19 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
        addrType = if (pFlag && wFlag)
                   then PreIndex
                   else if (pFlag && not wFlag)
                   then NoIndex
                   else PostIndex
-- Scaled register offset/index
decodeAddressingMode2 x
  | (x .&. 0x00000FF0 /= 0) = AddrMode2_3 addrType uFlag rn rm immediate shift
  where pFlag = x `testBit` 24
        wFlag = x `testBit` 21
        uFlag = x `testBit` 23
        rn = register (fromIntegral $ bitRange 16 19 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
        immediate = pure (fromIntegral $ bitRange 7 11 x)
        shift = fromIntegral $ bitRange 5 6 x
        addrType = if (pFlag && wFlag)
                   then PreIndex
                   else if (pFlag && not wFlag)
                   then NoIndex
                   else PostIndex

-- Immediate offset/index
decodeAddressingMode3 x
  | (x .&. 0x00400090) == 0x00400090 = AddrMode3_1 addrType uFlag rn immedH immedL
  where pFlag = x `testBit` 24
        wFlag = x `testBit` 21
        uFlag = x `testBit` 23
        rn = register (fromIntegral $ bitRange 16 19 x)
        immedH = pure $ bitRange 8 11 x
        immedL = pure $ bitRange 0 3 x
        addrType = if (pFlag && wFlag)
                   then PreIndex
                   else if (pFlag && not wFlag)
                   then NoIndex
                   else PostIndex
decodeAddressingMode3 x
  | (x .&. 0x00400090) == 0x00000090 = AddrMode3_2 addrType uFlag rn rm
  where pFlag = x `testBit` 24
        wFlag = x `testBit` 21
        uFlag = x `testBit` 23
        rn = register (fromIntegral $ bitRange 16 19 x)
        rm = register (fromIntegral $ bitRange 0 3 x)
        addrType = if (pFlag && wFlag)
                   then PreIndex
                   else if (pFlag && not wFlag)
                   then NoIndex
                   else PostIndex
