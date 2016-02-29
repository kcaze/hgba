module Mutable where

type Get s a = s -> a
type Set s a = s -> Get s a -> s
data Mutable s a = Mutable { get :: Get s a, set :: Set s a}
