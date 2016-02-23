module Util where

import Data.Bits
import Data.Word
import Data.Int

signExtend :: Int -> Int -> Word32 -> Word32
signExtend from to n = foldr (.|.) n' bs
  where n' = n `shift` (32 - from) `shift` (from - 32)
        b = n' .&. bit (from - 1)
        bs = map (shift b) $ [1 .. (to - from)]

testMask :: Word32 -> Word32 -> Bool
testMask word mask = word .&. mask == mask

arithmeticShiftR :: Word32 -> Int -> Word32
arithmeticShiftR word n = fromIntegral word'
  where word' = shiftR (fromIntegral word :: Int32) n

logicalShiftL :: Word32 -> Int -> Word32
logicalShiftL = shiftL

logicalShiftR :: Word32 -> Int -> Word32
logicalShiftR = shiftR
