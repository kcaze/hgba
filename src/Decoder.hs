module Decoder where

import Data.Bits
import Data.Word
import Prelude hiding (EQ, LT, GT)
import Types
import Util

decodeCond :: Word32 -> ConditionCode
decodeCond w = decodeCond' (w `shiftR` 28)
  where decodeCond' 0x0 = EQ
        decodeCond' 0x1 = NE
        decodeCond' 0x2 = HS
        decodeCond' 0x3 = LO
        decodeCond' 0x4 = MI
        decodeCond' 0x5 = PL
        decodeCond' 0x6 = VS
        decodeCond' 0x7 = VC
        decodeCond' 0x8 = HI
        decodeCond' 0x9 = LS
        decodeCond' 0xA = GE
        decodeCond' 0xB = LT
        decodeCond' 0xC = GT
        decodeCond' 0xD = LE
        decodeCond' 0xE = AL
        decodeCond' 0xF = AL -- 0b1111 is technically undefined

decodeInstruction :: Word32 -> Maybe Instruction
decodeInstruction x
  | testMask x 0x0A000000 = d decodeB
  | testMask x 0x012FFF10 = d decodeBX
  | otherwise = Nothing
  where condition = decodeCond x
        d :: (Word32 -> RawInstruction) -> Maybe Instruction
        d f = Just $ Instruction condition (f x)

decodeB x = B (x `testBit` 24) (x .&. 0x00FFFFFF)
decodeBX x = BX (x .&. 0x0000000F)
