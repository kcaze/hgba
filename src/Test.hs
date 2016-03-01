module Test where

import Bits
import CPU
import Instruction

instructions :: [Instruction] -> Execute
instructions [] = fromFunction id
instructions (x:xs) = fromFunction (execute (instruction x)
                                .>> execute (instructions xs))

program = [
    (Instruction al (MOV SFlagOff r0 (I_operand (pure 0xFF) (pure 0x00))))
  ]
