module Instruction where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word
import Bits
import CPU

data Shifter = I_operand (Immediate Word32) (Immediate Int)
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
data SFlag = SFlagOff | SFlagOn | SFlagOnR15

data Instruction = Instruction Flag RawInstruction
data RawInstruction = AND SFlag Register Register Shifter
                    | B Bool (Immediate Word32)
                    | BX Register
                    | EOR SFlag Register Register Shifter
                    | SUB SFlag Register Register Shifter
instruction :: Instruction -> Execute
instruction (Instruction c r) = _if c % raw r ! _id

raw :: RawInstruction -> Execute

------------------------
-- Branch instructions--
------------------------
raw (B lFlag immed) = (_if (pure lFlag) % set lr (pc - instructionSize) ! _id)
                  .>> (set pc (pc + ((signExtend 24 30 .$ immed) .<! 2)))
raw (BX rm) = set tFlag (rm .|?| 0)
          .>> set pc (rm .& 0xFFFFFFFE)

----------------------------------
-- Data-processing instructions --
----------------------------------
-- Shared logic across instructions is abstracted in dataProcess.
raw (AND s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = (.&)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = shifterCarry shift
        v = vFlag
raw (EOR s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = (.^)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = shifterCarry shift
        v = vFlag
raw (SUB s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = (.-)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = _not $ _borrowFrom rn (shifterOperand shift)
        v = _overflowFromSub rn (shifterOperand shift)

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
shifter :: Shifter -> Immediate (Word32, Bool)
shifterOperand :: Shifter -> Immediate Word32
shifterCarry :: Shifter -> Immediate Bool

shifterOperand s = fst .$ shifter s
shifterCarry s = snd .$ shifter s

shifter (I_operand immed_8 rotate_imm) = do {
  _if (rotate_imm .== 0) % _pair operand cFlag
  !                       _pair operand (operand .|?| 31)
} where operand = immed_8 `_rotateRight` (rotate_imm * 2)

shifter (R_operand rm) = _pair rm cFlag

shifter (R_LSL_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % _pair rm cFlag
  !                       _pair (rm .<! shift_imm) (rm .|?| (32 - shift_imm))
}

shifter (R_LSL_R_operand rm rs) = do {
  _if  (rs' .== 0)  % _pair rm cFlag
  !_if (rs' .< 32)  % _pair (rm .<! rs') (rm .|?| (32 - rs'))
  !_if (rs' .== 32) % _pair 0 (rm .|?| 0)
  !                   _pair 0 (pure False)
} where rs' = fromIntegral .$ (bitRange 0 7) .$ rs

shifter (R_LSR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % _pair rm (rm .|?| 31)
  !                       _pair (rm .!> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (R_LSR_R_operand rm rs) = do {
  _if  (rs' .== 0)  % _pair rm cFlag
  !_if (rs' .< 32)  % _pair (rm .!> rs') (rm .|?| (rs' - 1))
  !_if (rs' .== 32) % _pair 0 (rm .|?| 31)
  !                   _pair 0 (pure False)
} where rs' = fromIntegral .$ (bitRange 0 7) .$ rs

shifter (R_ASR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % (
    _if (not .$ (rm .|?| 31)) % _pair 0 (rm .|?| 31)
    !                           _pair 0xFFFFFFFF (rm .|?| 31)
  ) ! _pair (rm .?> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (R_ASR_R_operand rm rs) = do {
  _if (rs' .== 0)  % _pair rm cFlag
  !_if (rs' .< 32) % _pair (rm .?> rs') (rm .|?| (rs' - 1))
  ! ( _if (not .$ (rm .|?| 31)) % _pair 0 (rm .|?| 31)
      !                           _pair 0xFFFFFFFF (rm .|?| 31))
} where rs' = fromIntegral .$ bitRange 0 7 .$ rs

shifter (R_ROR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0)  % shifter (R_RRX_operand rm)
  !                        _pair (rm .@> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (R_ROR_R_operand rm rs) = do {
  _if (rs' .== 0)   % _pair rm cFlag
  !_if (rs'' .== 0) % _pair rm (rm .|?| 31)
  !                   _pair (rm .@> rs'') (rm .|?| (rs'' -1))
} where rs'  = fromIntegral .$ bitRange 0 7 .$ rs
        rs'' = fromIntegral .$ bitRange 0 4 .$ rs

shifter (R_RRX_operand rm) = do {
  _pair ((c .<! 31) .| (rm .!> 1)) (rm .|?| 0)
} where c = _if (cFlag) % 1 ! 0

dataProcess :: SFlag -> Register -> Register -> Shifter
            -> (Register -> Immediate Word32 -> Immediate Word32)
            -> Flag -> Flag -> Flag -> Flag
            -> Execute
dataProcess sFlag rd rn s op nVal zVal cVal vVal =
  set rd (rn `op` shifterOperand s)
  .>> (case sFlag of SFlagOff -> _id
                     SFlagOnR15 -> set cpsr spsr
                     SFlagOn -> (set nFlag nVal
                            .>>  set zFlag zVal
                            .>>  set cFlag cVal
                            .>>  set vFlag vVal))
