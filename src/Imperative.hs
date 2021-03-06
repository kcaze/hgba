module Imperative where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word

class Gettable t where
  get :: (t s a) -> (s -> a)
  fromFunction :: (s -> a) -> (t s a)

-- Instances for functions
instance Gettable (->) where
  get = id
  fromFunction = id

instance (Num a) => Num (s -> a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger n = fromFunction (const . fromInteger $ n)

-- Generic Value type and instances
data Value s a = Immutable (s -> a)
               | Immediate a
               | Register Int (s -> a) (a -> s -> s)
               | Mutable (s -> a) (a -> s -> s)

set :: Value s a -> (Value s a -> Value s s)
set (Mutable _ s) = \value -> fromFunction (\state -> s (get value state) state)
set (Register _ _ s) = \value -> fromFunction (\state -> s (get value state) state)
set _ = error "Attempting to set immutable value."

instance Gettable (Value) where
  get (Immutable g) = g
  get (Immediate x) = const x
  get (Register _ g _) = g
  get (Mutable g _) = g
  fromFunction = Immutable

instance Functor (Value s) where
  fmap f (Immediate x) = Immediate $ f x
  fmap f x = Immutable $ fmap f (get x)

instance Applicative (Value s) where
  pure x = Immediate x
  (Immediate x) <*> (Immediate y) = Immediate (x y)
  x <*> y = Immutable (get x <*> get y)

instance Monad (Value s) where
  x >>= y = fromFunction (\s -> get (y (get x s)) s)

instance (Num a) => Num (Value s a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger n = pure (fromInteger n)

instance (Show a) => Show (Value s a) where
  show (Immutable _) = "<Immutable>"
  show (Immediate x) = "<Immediate " ++ show x ++ ">"
  show (Register n _ _)  = "<Register " ++ show n ++ ">"
  show (Mutable _ _)  = "<Mutable>"

instance (Eq a) => Eq (Value s a) where
  (Immediate x) == (Immediate y) = x == y
  (Register n _ _) == (Register m _ _) = n == m
  _ == _ = False

-- Various lifted operators
_if :: Gettable g => g s Bool -> g s a -> g s a -> g s a
_if cond a b = fromFunction $ \s -> if get cond s then get a s else get b s

_id :: Gettable g => g a a
_id = fromFunction id

_pair :: Applicative f => f a -> f b -> f (a, b)
_pair = liftA2 (,)

(%) = ($) -- "then" operator
(!) = ($) -- "else" operator
infixl 1 %
infixr 0 !

(.>>) :: Gettable g => g a b -> g b c -> g a c
x .>> y = fromFunction $! (get y) . (get x)

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

(.+) :: (Num a, Applicative f) => f a -> f a -> f a
(.+) = liftA2 (+)
(.-) :: (Num a, Applicative f) => f a -> f a -> f a
(.-) = liftA2 (-)
(.*) :: (Num a, Applicative f) => f a -> f a -> f a
(.*) = liftA2 (*)

(.|) :: (Bits a, Applicative f) => f a -> f a -> f a
(.|) = liftA2 (.|.)
(.&) :: (Bits a, Applicative f) => f a -> f a -> f a
(.&) = liftA2 (.&.)
(.^) :: (Bits a, Applicative f) => f a -> f a -> f a
(.^) = _xor

(|?|) :: (Bits a) => a -> Int -> Bool
(|?|) = testBit
(.|?|) :: (Bits a, Applicative f) => f a -> f Int -> f Bool
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

-- Note: Most implementations here are naive and could be
--       more efficient for specific instances. Optimize
--       later if necessary.
class (Num a, Ord a, Integral a, FiniteBits a) => Bits' a where
  bitLength :: a -> Int
  bits :: Int -> Int -> a
  bitRange :: Int -> Int -> a -> a
  toggleBit :: Int -> Bool -> a -> a
  testMask :: a -> a -> Bool
  logicalShiftLeft :: a -> Int -> a
  logicalShiftRight :: a -> Int -> a
  arithmeticShiftRight :: a -> Int -> a
  rotateLeft :: a -> Int -> a
  rotateRight :: a -> Int -> a
  signExtend :: Int -> Int -> a -> a
  carryFrom :: a -> a -> Bool
  borrowFrom :: a -> a -> Bool
  overflowFromAdd :: a -> a -> Bool
  overflowFromSub :: a -> a -> Bool

  bitLength = finiteBitSize
  bits l h
    -- | h >= bitLength (0 :: a) || l > h || l < 0 || h < 0 = 0
    | l == 0 = bit (h+1) - 1
    | otherwise = bits 0 h `xor` bits 0 (l - 1)
  bitRange l h w = (w .&. bits l h) `logicalShiftRight` l
  toggleBit n b w = if b then setBit w n else clearBit w n
  testMask x y = (x .&. y) /= 0
  logicalShiftLeft = shiftL
  logicalShiftRight w n = shiftR w n .&. bits 0 (bitLength w - 1 - n)
  arithmeticShiftRight w n = shiftR w n .&. bits 0 (bitLength w - 1 - n)
                             .|. (if testBit w (bitLength w - 1)
                                  then bits (bitLength w - n) (bitLength w - 1)
                                  else 0)
  rotateLeft = rotateL
  rotateRight = rotateR
  signExtend from to n = (n .&. bits 0 (from - 1)) .|. b
    where b = if testBit n (from - 1) then bits from (to - 1) else 0
  carryFrom x y = x' + y' >= 2^(bitLength x)
    where x' = fromIntegral x :: Integer
          y' = fromIntegral y :: Integer
  borrowFrom x y = y > x
  overflowFromAdd x y = (xsign == ysign) && (xsign /= zsign)
    where xsign = testBit x (bitLength x - 1)
          ysign = testBit y (bitLength x - 1)
          zsign = testBit (x + y) (bitLength x - 1)
  overflowFromSub x y = (xsign /= ysign) && (xsign /= zsign)
    where xsign = testBit x (bitLength x - 1)
          ysign = testBit y (bitLength x - 1)
          zsign = testBit (x - y) (bitLength x - 1)

instance Bits' Word32
instance Bits' Int32
instance Bits' Word64
instance Bits' Int64

_xor :: (Bits a, Applicative f) => f a -> f a -> f a
_not :: Applicative f => f Bool -> f Bool
_testBit :: (Bits a, Applicative f) => f a -> f Int -> f Bool
_testMask :: (Bits' a, Applicative f) => f a -> f a -> f Bool
_bits :: (Bits' a, Applicative f) => Int -> Int -> f a
_bitRange :: (Bits' a, Applicative f) => Int -> Int -> f a -> f a
_logicalShiftLeft :: (Bits' a, Applicative f) => f a -> f Int -> f a
_logicalShiftRight :: (Bits' a, Applicative f) => f a -> f Int -> f a
_arithmeticShiftRight :: (Bits' a, Applicative f) => f a -> f Int -> f a
_rotateLeft :: (Bits' a, Applicative f) => f a -> f Int -> f a
_rotateRight :: (Bits' a, Applicative f) => f a -> f Int -> f a
_carryFrom :: (Bits' a, Applicative f) => f a -> f a -> f Bool
_borrowFrom :: (Bits' a, Applicative f) => f a -> f a -> f Bool
_overflowFromAdd :: (Bits' a, Applicative f) => f a -> f a -> f Bool
_overflowFromSub :: (Bits' a, Applicative f) => f a -> f a -> f Bool

_xor = liftA2 xor
_not = fmap not
_testBit = liftA2 testBit
_testMask = liftA2 testMask
_bits x y = pure $ bits x y
_bitRange x y = fmap $ bitRange x y
_logicalShiftLeft = liftA2 logicalShiftLeft
_logicalShiftRight = liftA2 logicalShiftRight
_arithmeticShiftRight = liftA2 arithmeticShiftRight
_rotateLeft = liftA2 rotateLeft
_rotateRight = liftA2 rotateRight
_carryFrom = liftA2 carryFrom
_borrowFrom = liftA2 borrowFrom
_overflowFromAdd = liftA2 overflowFromAdd
_overflowFromSub = liftA2 overflowFromSub
