module Cond where

import Mutable

_if :: Get s Bool -> (Bool -> Get s a) -> Get s a
_if cond res state = (res . cond $ state) $ state

_then :: Get s a -> Get s a -> Bool -> Get s a
_then f g cond = if cond then f else g

_else = id
_elseif = _if

(.==.) :: Eq a => Get s a -> Get s a -> Get s Bool
(.==.) f g state = f state == g state

(.!=.) :: Eq a => Get s a -> Get s a -> Get s Bool
(.!=.) f g cpu = f cpu /= g cpu
