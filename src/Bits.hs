module Bits where

import Data.Bits
import Data.Int
import Data.Word

(.^.) :: Word32 -> Word32 -> Word32
(.^.) = xor
(.<<.) :: Word32 -> Int -> Word32
(.<<.) = flip logicalShiftL
(.>>.) :: Word32 -> Int -> Word32
(.>>.) = flip logicalShiftR
(.>>>.) :: Word32 -> Int -> Word32
(.>>>.) = flip arithmeticShiftR
(.<@<.) :: Word32 -> Int -> Word32
(.<@<.) = rotateL
(.>@>.) :: Word32 -> Int -> Word32
(.>@>.) = rotateR

logicalShiftL :: Int -> Word32 -> Word32
logicalShiftR :: Int -> Word32 -> Word32
arithmeticShiftR :: Int -> Word32 -> Word32
logicalShiftL n w = fromIntegral $ (fromIntegral w :: Word32) `shiftL` n
logicalShiftR n w = fromIntegral $ (fromIntegral w :: Word32) `shiftR` n
arithmeticShiftR n w = fromIntegral $ (fromIntegral w :: Int32) `shiftR` n
rotateL' :: Int -> Word32 -> Word32
rotateL' = flip rotateL
rotateR' :: Int -> Word32 -> Word32
rotateR' = flip rotateR

bitRange :: Int -> Int -> Word32 -> Word32
bitRange low high w
  | low > high = 0
  | otherwise = w .<<. (31 - high) .>>. (31 - (high - low))
