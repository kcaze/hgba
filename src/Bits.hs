module Bits where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word

class Gettable t where
  get :: (t s a) -> (s -> a)
  fromFunction :: (s -> a) -> (t s a)

data Value s a = Immutable (s -> a)
               | Mutable (s -> a) (a -> s -> s)

set :: Value s a -> (Value s a -> Value s s)
set (Mutable _ s) = \value -> fromFunction (\state -> s (get value state) state)
set _ = error "Attempting to set immutable value."

instance Gettable (->) where
  get = id
  fromFunction = id

instance Gettable (Value) where
  get (Immutable g) = g
  get (Mutable g _) = g
  fromFunction = Immutable

instance Functor (Value s) where
  fmap f x = Immutable $ fmap f (get x)

instance Applicative (Value s) where
  pure x = Immutable (const x)
  x <*> y = Immutable (get x <*> get y)

instance (Num a) => Num (s -> a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger n = fromFunction (const . fromInteger $ n)

instance (Num a) => Num (Value s a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger n = fromFunction (const . fromInteger $ n)

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
x .>> y = fromFunction $ (get y) . (get x)

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
class (Num a, FiniteBits a) => Bits' a where
  bitLength :: a -> Int
  bits :: Int -> Int -> a
  bitRange :: Int -> Int -> a -> a
  logicalShiftLeft :: a -> Int -> a
  logicalShiftRight :: a -> Int -> a
  arithmeticShiftRight :: a -> Int -> a
  rotateLeft :: a -> Int -> a
  rotateRight :: a -> Int -> a

  {--signExtend :: Int -> Int -> a -> a
  carryFrom :: a -> a -> Bool
  borrowFrom :: a -> a -> Bool
  overflowFromAdd :: a -> a -> Bool
  overflowFromSub :: a -> a -> Bool--}

  bitLength = finiteBitSize 
  bits l h
    | l > h = 0
    | otherwise = bit l .|. bits (l+1) h
  bitRange l h w = w .&. bits l h
  logicalShiftLeft = shiftL
  logicalShiftRight w n = shiftR w n .&. bits 0 (bitLength w - 1 - n)
  arithmeticShiftRight w n = shiftR w n .&. bits 0 (bitLength w - 1 - n)
                             .|. (if testBit w (bitLength w - 1)
                                  then bits (bitLength w - n) (bitLength w - 1)
                                  else 0)
  rotateLeft = rotateL
  rotateRight = rotateR

instance Bits' Word32
instance Bits' Word64

{--
instance Bits' Word32 where
  logicalShiftLeft w n = fromIntegral $ (fromIntegral w :: Word32) `shiftL` n
  logicalShiftRight w n = fromIntegral $ (fromIntegral w :: Word32) `shiftR` n
  arithmeticShiftRight w n = fromIntegral $ (fromIntegral w :: Int32) `shiftR` n
  rotateLeft = rotateL
  rotateRight = rotateR
  bitRange low high w
    | low > high = 0
    | otherwise = w <! (31 - high) !> (31 - (high - low))
  signExtend from to n = foldr (.|.) n' bs
    where n' = n `shift` (32 - from) `shift` (from - 32)
          b = n' .&. bit (from - 1)
          bs = map (shift b) $ [1 .. (to - from)]
  carryFrom x y = x' + y' >= 2^32
    where x' = fromIntegral x :: Integer
          y' = fromIntegral y :: Integer
  borrowFrom x y = y > x
  overflowFromAdd x y = (xsign == ysign) && (xsign /= zsign)
    where xsign = testBit x 31
          ysign = testBit y 31
          zsign = testBit (x + y) 31
  overflowFromSub x y = (xsign /= ysign) && (xsign /= zsign)
    where xsign = testBit x 31
          ysign = testBit y 31
          zsign = testBit (x - y) 31
--}

_xor :: (Bits a, Applicative f) => f a -> f a -> f a
_not :: Applicative f => f Bool -> f Bool
_testBit :: (Bits a, Applicative f) => f a -> f Int -> f Bool
_logicalShiftLeft :: Applicative f => f Word32 -> f Int -> f Word32
_logicalShiftRight :: Applicative f => f Word32 -> f Int -> f Word32
_arithmeticShiftRight :: Applicative f => f Word32 -> f Int -> f Word32
_rotateLeft :: Applicative f => f Word32 -> f Int -> f Word32
_rotateRight :: Applicative f => f Word32 -> f Int -> f Word32
{--_carryFrom :: Applicative f => f Word32 -> f Word32 -> f Bool
_borrowFrom :: Applicative f => f Word32 -> f Word32 -> f Bool
_overflowFromAdd :: Applicative f => f Word32 -> f Word32 -> f Bool
_overflowFromSub :: Applicative f => f Word32 -> f Word32 -> f Bool--}

_xor = liftA2 xor
_not = fmap not
_testBit = liftA2 testBit
_logicalShiftLeft = liftA2 logicalShiftLeft
_logicalShiftRight = liftA2 logicalShiftRight
_arithmeticShiftRight = liftA2 arithmeticShiftRight
_rotateLeft = liftA2 rotateLeft
_rotateRight = liftA2 rotateRight
{--_carryFrom = liftA2 carryFrom
_borrowFrom = liftA2 borrowFrom
_overflowFromAdd = liftA2 overflowFromAdd
_overflowFromSub = liftA2 overflowFromSub--}
