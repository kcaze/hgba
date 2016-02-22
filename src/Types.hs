module Types where

import qualified Data.Map.Strict as Map
import Data.Word

type Byte = Word8
type Halfword = Word16
type Word = Word32

type Address = Word32
type Register = Word32
type AddressSpace = Map.Map Address Byte
