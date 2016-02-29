module Bits where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word

instance Num a => Num (s -> a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = const . fromInteger

_if :: (s -> Bool) -> (s -> a) -> (s -> a) -> (s -> a)
_if cond a b = \s -> if cond s then a s else b s

(%) = ($) -- "then" operator
(!) = ($) -- "else" operator
infixl 1 %
infixr 0 !

pair :: Applicative f => f a -> f b -> f (a, b)
pair = liftA2 (,)

(.$) :: Functor f => (a -> b) -> f a -> f b
(.$) = (<$>)

(.==) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(./=) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(.&&) :: Applicative f => f Bool -> f Bool -> f Bool
(.||) :: Applicative f => f Bool -> f Bool -> f Bool
(.<) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<=) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>=) :: (Ord a, Applicative f) => f a -> f a -> f Bool

(.==) = liftA2 (==)
(./=) = liftA2 (/=)
(.&&) = liftA2 (&&)
(.||) = liftA2 (||)
(.<) = liftA2 (<)
(.>) = liftA2 (>)
(.<=) = liftA2 (<=)
(.>=) = liftA2 (>=)

(.|) :: Applicative f => f Word32 -> f Word32 -> f Word32
(.|) = liftA2 (.|.)
(.&) :: Applicative f => f Word32 -> f Word32 -> f Word32
(.&) = liftA2 (.&.)

(|?|) :: Word32 -> Int -> Bool
(|?|) = testBit
(.|?|) :: Applicative f => f Word32 -> f Int -> f Bool
(.|?|) = _testBit

(<!) :: Word32 -> Int -> Word32
(<!) = logicalShiftLeft
(.<!) :: Applicative f => f Word32 -> f Int -> f Word32
(.<!) = _logicalShiftLeft

(!>) :: Word32 -> Int -> Word32
(!>) = logicalShiftRight
(.!>) :: Applicative f => f Word32 -> f Int -> f Word32
(.!>) = _logicalShiftRight

(?>) :: Word32 -> Int -> Word32
(?>) = arithmeticShiftRight
(.?>) :: Applicative f => f Word32 -> f Int -> f Word32
(.?>) = _arithmeticShiftRight

(<@) :: Word32 -> Int -> Word32
(<@) = rotateLeft
(.<@) :: Applicative f => f Word32 -> f Int -> f Word32
(.<@) = _rotateLeft

(@>) :: Word32 -> Int -> Word32
(@>) = rotateRight
(.@>) :: Applicative f => f Word32 -> f Int -> f Word32
(.@>) = _rotateRight

logicalShiftLeft :: Word32 -> Int -> Word32
logicalShiftRight :: Word32 -> Int -> Word32
arithmeticShiftRight :: Word32 -> Int -> Word32
rotateLeft :: Word32 -> Int -> Word32
rotateRight :: Word32 -> Int -> Word32
bitRange :: Int -> Int -> Word32 -> Word32

logicalShiftLeft w n = fromIntegral $ (fromIntegral w :: Word32) `shiftL` n
logicalShiftRight w n = fromIntegral $ (fromIntegral w :: Word32) `shiftR` n
arithmeticShiftRight w n = fromIntegral $ (fromIntegral w :: Int32) `shiftR` n
rotateLeft = rotateL
rotateRight = rotateR
bitRange low high w
  | low > high = 0
  | otherwise = w <! (31 - high) !> (31 - (high - low))


_xor :: Applicative f => f Word32 -> f Word32 -> f Word32
_testBit :: Applicative f => f Word32 -> f Int -> f Bool
_logicalShiftLeft :: Applicative f => f Word32 -> f Int -> f Word32
_logicalShiftRight :: Applicative f => f Word32 -> f Int -> f Word32
_arithmeticShiftRight :: Applicative f => f Word32 -> f Int -> f Word32
_rotateLeft :: Applicative f => f Word32 -> f Int -> f Word32
_rotateRight :: Applicative f => f Word32 -> f Int -> f Word32

_xor = liftA2 xor
_testBit = liftA2 testBit
_logicalShiftLeft = liftA2 logicalShiftLeft
_logicalShiftRight = liftA2 logicalShiftRight
_arithmeticShiftRight = liftA2 arithmeticShiftRight
_rotateLeft = liftA2 rotateLeft
_rotateRight = liftA2 rotateRight
