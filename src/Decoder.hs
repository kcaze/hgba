module Decoder where

import Types
import qualified Instruction as I

decode :: Word -> Maybe I.Instruction
decode 0 = Just I.Instruction I.AL I.B
decode x = Nothing
