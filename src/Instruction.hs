{-# LANGUAGE BinaryLiterals #-}
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

data AddressingModeType = NoIndex | PreIndex | PostIndex
data AddressingMode = AddrMode1 AddressingModeType Bool Register Word32
                    | AddrMode2 AddressingModeType Bool Register Register
                    | AddrMode3 AddressingModeType Bool Register Register Int Int
-- Evaluating an address mode results in an address and an execute.
-- The execute is necessary for base register write-back.
addressMode :: AddressingMode -> (Immediate Word32, Execute)
addressMode (AddrMode1 addrType uflag rn offset) = 
  addressMode' addrType rn address
  where address = if uflag then rn .+ pure offset else rn .- pure offset
addressMode (AddrMode2 addrType uflag rn rm) =
  addressMode' addrType rn address
  where address = if uflag then rn .+ rm else rn .- rm
addressMode (AddrMode3 addrType uflag rn rm immediate shift) =
  addressMode' addrType rn address
  where index = case shift of
                  0b00 -> (rm .<! pure immediate)
                  0b01 -> if (immediate == 0) then 0 else (rm .!> pure immediate)
                  0b10 -> if (immediate == 0) then (
                            _if (rm .|?| 31) % 0xFFFFFFFF ! 0
                          ) else (rm .?> pure immediate)
                  0b11 -> if (immediate == 0) then (
                            (cBit .<! 31) .| (rm .!> 1)
                          ) else (rm .@> pure immediate)
        address = if uflag then rn .+ index else rn .- index

addressMode' :: AddressingModeType ->
                Register -> Immediate Word32 ->
                (Immediate Word32, Execute)
addressMode' NoIndex _ address = (address, _id)
addressMode' PreIndex rn address = (address, set rn address)
addressMode' PostIndex rn address = (rn, set rn address)

data Instruction = Instruction Flag RawInstruction
data RawInstruction = ADC SFlag Register Register Shifter
                    | ADD SFlag Register Register Shifter
                    | AND SFlag Register Register Shifter
                    | B Bool (Immediate Word32)
                    | BIC SFlag Register Register Shifter
                    | BX Register
                    | CMN Register Shifter
                    | CMP Register Shifter
                    | EOR SFlag Register Register Shifter
                    | ORR SFlag Register Register Shifter
                    | MLA SFlag Register Register Register Register
                    | MOV SFlag Register Shifter
                    | MRS Bool Register
                    | MSR Bool (Immediate Word32) Shifter
                    | MUL SFlag Register Register Register
                    | MVN SFlag Register Shifter
                    | RSB SFlag Register Register Shifter
                    | RSC SFlag Register Register Shifter
                    | SBC SFlag Register Register Shifter
                    | SMLAL SFlag Register Register Register Register
                    | SMULL SFlag Register Register Register Register
                    | SUB SFlag Register Register Shifter
                    | TEQ Register Shifter
                    | TST Register Shifter
                    | UMLAL SFlag Register Register Register Register
                    | UMULL SFlag Register Register Register Register
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
raw (RSB s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = flip (.-)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = _not $ _borrowFrom (shifterOperand shift) rn
        v = _overflowFromSub (shifterOperand shift) rn
raw (ADD s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = (.+)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = _carryFrom rn (shifterOperand shift)
        v = _overflowFromAdd rn (shifterOperand shift)
raw (ADC s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = \r operand -> (r .+ operand .+ cBit)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = _carryFrom rn (shifterOperand shift .+ cBit)
        v = _overflowFromAdd rn (shifterOperand shift .+ cBit)
raw (SBC s rd rn shift) = dataProcess s rd rn shift op n z c v
  where cBit' = fromFlag $ _not cFlag
        op = \r operand -> (r .- (operand .+ cBit'))
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = _borrowFrom rn (shifterOperand shift .+ cBit')
        v = _overflowFromSub rn (shifterOperand shift .+ cBit')
raw (RSC s rd rn shift) = dataProcess s rd rn shift op n z c v
  where cBit' = fromFlag $ _not cFlag
        op = \r operand -> (operand .- (r .+ cBit'))
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = _borrowFrom (shifterOperand shift) (rn .+ cBit')
        v = _overflowFromSub (shifterOperand shift) (rn .+ cBit')
raw (TST rn shift) = set nFlag (aluOut .|?| 31)
                 .>> set zFlag (aluOut .== 0)
                 .>> set cFlag (shifterCarry shift)
  where aluOut = rn .& (shifterOperand shift)
raw (TEQ rn shift) = set nFlag (aluOut .|?| 31)
                 .>> set zFlag (aluOut .== 0)
                 .>> set cFlag (shifterCarry shift)
  where aluOut = rn .^ (shifterOperand shift)
raw (CMP rn shift) = set nFlag (aluOut .|?| 31)
                 .>> set zFlag (aluOut .== 0)
                 .>> set cFlag (_not $ _borrowFrom rn (shifterOperand shift))
                 .>> set vFlag (_overflowFromSub rn (shifterOperand shift))
  where aluOut = rn .- (shifterOperand shift)
raw (CMN rn shift) = set nFlag (aluOut .|?| 31)
                 .>> set zFlag (aluOut .== 0)
                 .>> set cFlag (_not $ _carryFrom rn (shifterOperand shift))
                 .>> set vFlag (_overflowFromAdd rn (shifterOperand shift))
  where aluOut = rn .+ (shifterOperand shift)
raw (ORR s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = (.|)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = shifterCarry shift
        v = vFlag
raw (MOV s rd shift) = dataProcess s rd rd shift op n z c v
  where op = \r operand -> operand
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = shifterCarry shift
        v = vFlag
raw (BIC s rd rn shift) = dataProcess s rd rn shift op n z c v
  where op = \r operand -> r .& (complement .$ operand)
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = shifterCarry shift
        v = vFlag
raw (MVN s rd shift) = dataProcess s rd rd shift op n z c v
  where op = \r operand -> complement .$ operand
        n = (rd .|?| 31)
        z = (rd .== 0)
        c = shifterCarry shift
        v = vFlag
-- Multiply instructions 
raw (MUL s rd rm rs) = set rd (rm .* rs)
                   .>> (case s of
                          SFlagOn -> (set nFlag (rd .|?| 31)
                                 .>>  set zFlag (rd .== 0))
                          _       -> _id)
raw (MLA s rd rm rs rn) = set rd ((rm .* rs) .+ rn)
                      .>> (case s of
                             SFlagOn -> (set nFlag (rd .|?| 31)
                                 .>>  set zFlag (rd .== 0))
                             _       -> _id)
raw (SMULL s rdlo rdhi rm rs) =
      set rdhi (to32 .$ _bitRange 32 63 prod)
  .>> set rdlo (to32 .$ _bitRange  0 31 prod)
  .>> (case s of
        SFlagOn -> (set nFlag (rdhi .|?| 31)
               .>>  set zFlag ((rdhi .== 0) .&& (rdlo .== 0)))
        _       -> _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral (fromIntegral x :: Int32) :: Int64
        to32 x = fromIntegral x :: Word32
raw (UMULL s rdlo rdhi rm rs) =
      set rdhi (to32 .$ _bitRange 32 63 prod)
  .>> set rdlo (to32 .$ _bitRange  0 31 prod)
  .>> (case s of
        SFlagOn -> (set nFlag (rdhi .|?| 31)
               .>>  set zFlag ((rdhi .== 0) .&& (rdlo .== 0)))
        _       -> _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral x :: Word64
        to32 x = fromIntegral x :: Word32
raw (SMLAL s rdlo rdhi rm rs) =
      set rdhi ((to32 .$ _bitRange 32 63 prod) .+ rdhi .+ carry)
  .>> set rdlo ((to32 .$ _bitRange  0 31 prod) .+ rdlo)
  .>> (case s of
        SFlagOn -> (set nFlag (rdhi .|?| 31)
               .>>  set zFlag ((rdhi .== 0) .&& (rdlo .== 0)))
        _       -> _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral (fromIntegral x :: Int32) :: Int64
        to32 x = fromIntegral x :: Word32
        carry = _if (_carryFrom (to32 .$ _bitRange 0 31 prod) rdlo) % 1 ! 0
raw (UMLAL s rdlo rdhi rm rs) =
      set rdhi ((to32 .$ _bitRange 32 63 prod) .+ rdhi .+ carry)
  .>> set rdlo ((to32 .$ _bitRange  0 31 prod) .+ rdlo)
  .>> (case s of
        SFlagOn -> (set nFlag (rdhi .|?| 31)
               .>>  set zFlag ((rdhi .== 0) .&& (rdlo .== 0)))
        _       -> _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral x :: Word64
        to32 x = fromIntegral x :: Word32
        carry = _if (_carryFrom (to32 .$ _bitRange 0 31 prod) rdlo) % 1 ! 0
-- Status register access instructions
raw (MRS rbit rd) = set rd psr
  where psr = if rbit then spsr else cpsr
raw (MSR rbit fieldMask shift) = 
  if not rbit then (
    let mask = _if (processorMode ./= pure User)
               %   (byteMask .& (userMask .| privMask))
               !   (byteMask .& userMask)
    in set cpsr ((cpsr .& (complement .$ mask)) .| (operand .& mask))
  ) else (
    let mask = byteMask .& (userMask .| privMask .| stateMask)
    in set spsr ((spsr .& (complement .$ mask)) .| (operand .& mask))
  )
  where operand = shifterOperand shift
        unallocMask = 0x0FFFFF00
        userMask = 0xF0000000
        privMask = 0x0000000F
        stateMask = 0x00000020
        byteMask = (_if (_testBit fieldMask 0) % 0x000000FF ! 0) .|
                   (_if (_testBit fieldMask 1) % 0x0000FF00 ! 0) .|
                   (_if (_testBit fieldMask 2) % 0x00FF0000 ! 0) .|
                   (_if (_testBit fieldMask 3) % 0xFF000000 ! 0)
                    

-- Condition codes
eq = zFlag
ne = _not zFlag
cs = cFlag
hs = cs
cc = _not cFlag
lo = cc
mi = nFlag
pl = _not nFlag
vs = vFlag
vc = _not vFlag
hi = cs .&& ne
ls = cc .&& eq
ge = mi .== vs
lt = mi ./= vs
gt = ne .&& ge
le = eq .|| lt
al :: Flag
al = pure True

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
