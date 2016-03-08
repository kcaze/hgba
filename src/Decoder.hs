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
  | testMask x 0x0A000000 = d decodeB
  | testMask x 0x012FFF10 = d decodeBX
  | otherwise = Nothing
  where condition = decodeCond (x `shiftR` 0x1C)
        d :: (Word32 -> Maybe RawInstruction) -> Maybe Instruction
        d f = case (f x) of
                (Just ri) -> (Just $ Instruction condition ri)
                Nothing   -> Nothing

decodeB x = Just $ B (x `testBit` 24) (pure $ x .&. 0x00FFFFFF)
decodeBX x = Just $ BX (register (fromIntegral $ x .&. 0x0000000F))
