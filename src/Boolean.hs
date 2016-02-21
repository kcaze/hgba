{-# LANGUAGE FlexibleInstances #-}
module Boolean where

import qualified Control.Applicative as A
import qualified Prelude as P

infixr 2 ||
infixr 3 &&

class Boolean b where
  true :: b
  false :: b
  not :: b -> b
  (&&) :: b -> b -> b
  (||) :: b -> b -> b
  eq :: b -> b -> b
  neq :: b -> b -> b

  true = not false
  false = not true
  x && y = not (not x || not y)
  x || y = not (not x && not y)
  x `eq` y = not (x `neq` y)
  x `neq` y = not (x `eq` y)

instance Boolean P.Bool where
  true = P.True
  not = P.not
  (&&) = (P.&&)
  eq = (P.==)

instance Boolean (e -> P.Bool) where
  true = P.const P.True
  not = (P.not P..)
  f && g = A.liftA2 (P.&&) f g
  eq = A.liftA2 (P.==)
