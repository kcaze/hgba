module Test where

import Bits
import CPU
import Instruction

instructions :: [Instruction] -> Execute
instructions [] = fromFunction id
instructions (x:xs) = fromFunction (execute (instruction x)
                                .>> execute (instructions xs))

program = [
      (Instruction al (MOV SFlagOff r0 (I_operand (pure 0x04) (pure 0x00))))
    , (Instruction al (MOV SFlagOff r0 (R_LSL_I_operand r0 (pure 0x18))))
    , (Instruction al (MOV SFlagOff r1 (I_operand (pure 0xD4) (pure 0x00))))
    , (Instruction al (ADD SFlagOff r0 r0 (R_operand r1)))
    , (Instruction al (STR r0 (AddrMode1 NoIndex False r0 0)))
    , (Instruction al (LDR r2 (AddrMode1 NoIndex False r0 2)))
  ]
