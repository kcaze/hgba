module Instruction where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word
import Bits
import CPU

type Register = CPU -> Word32
type Immediate a = CPU -> a

data ShifterOperand = I_operand (Immediate Word32) (Immediate Int)
                    | R_operand Register
                    | R_LSL_I_operand Register (Immediate Int)
                    | R_LSL_R_operand Register Register
                    | R_LSR_I_operand Register (Immediate Int)
                    | R_LSR_R_operand Register Register
                    | R_ASR_I_operand Register (Immediate Int)
                    | R_ASR_R_operand Register Register
                    | R_ROR_I_operand Register (Immediate Int)
                    | R_ROR_R_operand Register Register
                    | R_RRX_operand Register

-- Condition codes
eq = zFlag
ne = not .$ zFlag
cs = cFlag
hs = cs
cc = not .$ cFlag
lo = cc
mi = nFlag
pl = not .$ nFlag
vs = vFlag
vc = not .$ vFlag
hi = cs .&& ne
ls = cc .&& eq
ge = mi .== vs
lt = mi ./= vs
gt = ne .&& ge
le = eq .|| lt
al = const True

-- Addressing Mode 1: Data-processing operands
shifter :: ShifterOperand -> (CPU -> (Word32, Bool))

shifter (I_operand immed_8 rotate_imm) = do {
  _if (rotate_imm .== 0) % pair operand cFlag
  !                       pair operand (operand .|?| 31)
} where operand = immed_8 `_rotateRight` (rotate_imm * 2)

shifter (R_operand rm) = rm `pair` cFlag

shifter (R_LSL_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % pair rm cFlag
  !                       pair (rm .<! shift_imm) (rm .|?| (32 - shift_imm))
}

shifter (R_LSL_R_operand rm rs) = do {
  _if  (rs' .== 0)  % pair rm cFlag
  !_if (rs' .< 32)  % pair (rm .<! rs') (rm .|?| (32 - rs'))
  !_if (rs' .== 32) % pair 0 (rm .|?| 0)
  !                   pair 0 (const False)
} where rs' = fromIntegral .$ (bitRange 0 7) .$ rs

shifter (R_LSR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % pair rm (rm .|?| 31)
  !                       pair (rm .!> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (R_LSR_R_operand rm rs) = do {
  _if  (rs' .== 0)  % pair rm cFlag
  !_if (rs' .< 32)  % pair (rm .!> rs') (rm .|?| (rs' - 1))
  !_if (rs' .== 32) % pair 0 (rm .|?| 31)
  !                   pair 0 (const False)
} where rs' = fromIntegral .$ (bitRange 0 7) .$ rs

shifter (R_ASR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % (
    _if (not .$ (rm .|?| 31)) % pair 0 (rm .|?| 31)
    !                           pair 0xFFFFFFFF (rm .|?| 31)
  ) ! pair (rm .?> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (R_ASR_R_operand rm rs) = do {
  _if (rs' .== 0)  % pair rm cFlag
  !_if (rs' .< 32) % pair (rm .?> rs') (rm .|?| (rs' - 1))
  ! ( _if (not .$ (rm .|?| 31)) % pair 0 (rm .|?| 31)
      !                           pair 0xFFFFFFFF (rm .|?| 31))
} where rs' = fromIntegral .$ bitRange 0 7 .$ rs

shifter (R_ROR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0)  % shifter (R_RRX_operand rm)
  !                        pair (rm .@> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (R_ROR_R_operand rm rs) = do {
  _if (rs' .== 0)   % pair rm cFlag
  !_if (rs'' .== 0) % pair rm (rm .|?| 31)
  !                   pair (rm .@> rs'') (rm .|?| (rs'' -1))
} where rs'  = fromIntegral .$ bitRange 0 7 .$ rs
        rs'' = fromIntegral .$ bitRange 0 4 .$ rs

shifter (R_RRX_operand rm) = do {
  pair ((c .<! 31) .| (rm .!> 1)) (rm .|?| 0)
} where c = _if (cFlag) % 1 ! 0
