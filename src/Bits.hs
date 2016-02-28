module Bits ( Bits'
            , Num'
            ) where

import Control.Applicative
import Data.Bits as B
import Data.Int
import Data.Word

-- These typeclass instances are general but they're just used for Get.
class Num' a where
  (.+.) :: a -> a -> a
  (.*.) :: a -> a -> a

instance Num b => Num' (a -> b) where
  (.+.) = liftA2 (+)
  (.*.) = liftA2 (*)

class Bits' a where
  (.&.) :: a -> a -> a    -- AND
  (.|.) :: a -> a -> a    -- OR
  (.^.) :: a -> a -> a    -- XOR
  (.~.) :: a -> a         -- NOT
  (.>>.) :: a -> a -> a   -- LSR
  (.<<.) :: a -> a -> a   -- LSL
  (.>>>.) :: a -> a -> a  -- ASR
  (.>@>.) :: a -> a -> a  -- ROR
  (.<@<.) :: a -> a -> a  -- ROL

instance (Integral b, Bits b) => Bits' (a -> b) where
  (.&.) = liftA2 (B..&.)
  (.|.) = liftA2 (B..|.)
  (.^.) = liftA2 xor
  (.~.) = liftA complement
  (.>>.) = liftA2 logicalShiftL
  (.<<.) = liftA2 logicalShiftR
  (.>>>.) = liftA2 arithmeticShiftR
  (.>@>.) = liftA2 rotateR'
  (.<@<.) = liftA2 rotateL'

logicalShiftL :: (Integral a, Bits a) => a -> a -> a
logicalShiftR :: (Integral a, Bits a) => a -> a -> a
arithmeticShiftR :: (Integral a, Bits a) => a -> a -> a
rotateR' :: (Integral a, Bits a) => a -> a -> a
rotateL' :: (Integral a, Bits a) => a -> a -> a
x `logicalShiftL` y = fromIntegral $
                      (fromIntegral x :: Word32) `shiftL` (fromIntegral y)
x `logicalShiftR` y = fromIntegral $
                      (fromIntegral x :: Word32) `shiftR` (fromIntegral y)
x `arithmeticShiftR` y = fromIntegral $
                         (fromIntegral x :: Int32) `shiftR` (fromIntegral y)
x `rotateR'` y = fromIntegral $
                 (fromIntegral x :: Int32) `rotateR` (fromIntegral y)
x `rotateL'` y = fromIntegral $
                 (fromIntegral x :: Int32) `rotateL` (fromIntegral y)
