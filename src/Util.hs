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

carryFrom :: Word32 -> Word32 -> Bool
carryFrom x y = x' + y' >= 2^32
  where x' = fromIntegral x :: Integer
        y' = fromIntegral y :: Integer

borrowFrom :: Word32 -> Word32 -> Bool
borrowFrom x y = y > x

overflowFromAdd :: Word32 -> Word32 -> Bool
overflowFromAdd x y = (xsign == ysign) && (xsign /= zsign)
  where xsign = testBit x 31
        ysign = testBit y 31
        zsign = testBit (x + y) 31

overflowFromSub :: Word32 -> Word32 -> Bool
overflowFromSub x y = (xsign /= ysign) && (xsign /= zsign)
  where xsign = testBit x 31
        ysign = testBit y 31
        zsign = testBit (x - y) 31
