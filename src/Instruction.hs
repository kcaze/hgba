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
             | LSL_I_operand Register (Immediate Int)
             | LSL_R_operand Register Register
             | LSR_I_operand Register (Immediate Int)
             | LSR_R_operand Register Register
             | ASR_I_operand Register (Immediate Int)
             | ASR_R_operand Register Register
             | ROR_I_operand Register (Immediate Int)
             | ROR_R_operand Register Register
             | RRX_operand Register
  deriving (Show, Eq)

data AddressingModeType = NoIndex | PreIndex | PostIndex
  deriving (Show, Eq)
data AddressingMode2 = AddrMode2_1 AddressingModeType Bool Register (Immediate Word32)
                     | AddrMode2_2 AddressingModeType Bool Register Register
                     | AddrMode2_3 AddressingModeType Bool Register Register (Immediate Int) Int
  deriving (Show, Eq)
data AddressingMode3 = AddrMode3_1 AddressingModeType Bool Register (Immediate Word32) (Immediate Word32)
                     | AddrMode3_2 AddressingModeType Bool Register Register
  deriving (Show, Eq)
data AddressingMode4 = IA Bool Register
                     | IB Bool Register
                     | DA Bool Register
                     | DB Bool Register
  deriving (Show, Eq)
-- Evaluating an address mode results in an address and an execute.
-- The execute is necessary for base register write-back.
addressMode2 :: AddressingMode2 -> (Immediate Word32, Execute)
addressMode2 (AddrMode2_1 addrType uflag rn offset) = 
  addressMode' addrType rn address
  where address = if uflag then rn .+ offset else rn .- offset
addressMode2 (AddrMode2_2 addrType uflag rn rm) =
  addressMode' addrType rn address
  where address = if uflag then rn .+ rm else rn .- rm
addressMode2 (AddrMode2_3 addrType uflag rn rm immediate shift) =
  addressMode' addrType rn address
  where index = case shift of
                  0b00 -> (rm .<! immediate)
                  0b01 -> (_if (immediate .== 0) % 0 ! (rm .!> immediate))
                  0b10 -> (_if (immediate .== 0) % (
                            _if (rm .|?| 31) % 0xFFFFFFFF ! 0
                          ) ! (rm .?> immediate))
                  0b11 -> (_if (immediate .== 0) % (
                            (cBit .<! 31) .| (rm .!> 1)
                          ) ! (rm .@> immediate))
        address = if uflag then rn .+ index else rn .- index

addressMode3 :: AddressingMode3 -> (Immediate Word32, Execute)
addressMode3 (AddrMode3_1 addrType uFlag rn immedH immedL) =
  addressMode' addrType rn address
  where offset = (immedH .<! 4) .| immedL
        address = if uFlag then rn .+ offset else rn .- offset
addressMode3 (AddrMode3_2 addrType uFlag rn rm) =
  addressMode' addrType rn address
  where address = if uFlag then rn .+ rm else rn .- rm

addressMode' :: AddressingModeType -> Register -> Immediate Word32 ->
                 (Immediate Word32, Execute)
addressMode' NoIndex _ address = (address, _id)
addressMode' PreIndex rn address = (address, set rn address)
addressMode' PostIndex rn address = (rn, set rn address)

-- Evaluating an address mode results in a start address and an execute.
-- The execute is necessary for base register write-back.
addressMode4 :: AddressingMode4 -> [Register] -> (Immediate Word32, Execute)
addressMode4 (IA wflag rn) registerList = (startAddress, writeBack)
  where bitsSet = pure (fromIntegral $ length registerList)
        startAddress = rn 
        writeBack = if wflag then set rn (rn .+ (bitsSet .* 4)) else _id
addressMode4 (IB wflag rn) registerList = (startAddress, writeBack)
  where bitsSet = pure (fromIntegral $ length registerList)
        startAddress = rn .+ 4
        writeBack = if wflag then set rn (rn .+ (bitsSet .* 4)) else _id
addressMode4 (DA wflag rn) registerList = (startAddress, writeBack)
  where bitsSet = pure (fromIntegral $ length registerList)
        startAddress = rn .- (bitsSet .* 4) .+ 4
        writeBack = if wflag then set rn (rn .- (bitsSet .* 4)) else _id
addressMode4 (DB wflag rn) registerList = (startAddress, writeBack)
  where bitsSet = pure (fromIntegral $ length registerList)
        startAddress = rn .- (bitsSet .* 4)
        writeBack = if wflag then set rn (rn .- (bitsSet .* 4)) else _id

data Instruction = Instruction Flag RawInstruction
  deriving (Show, Eq)
data RawInstruction = ADC Bool Register Register Shifter
                    | ADD Bool Register Register Shifter
                    | AND Bool Register Register Shifter
                    | B Bool (Immediate Word32)
                    | BIC Bool Register Register Shifter
                    | BX Register
                    | CMN Register Shifter
                    | CMP Register Shifter
                    | EOR Bool Register Register Shifter
                    | LDM1 AddressingMode4 [Register]
                    | LDM2 AddressingMode4 [Register]
                    | LDM3 AddressingMode4 [Register]
                    | LDR Register AddressingMode2
                    | LDRB Register AddressingMode2
                    | LDRBT Register AddressingMode2
                    | LDRH Register AddressingMode3
                    | LDRT Register AddressingMode2
                    | LDRSB Register AddressingMode3
                    | LDRSH Register AddressingMode3
                    | MLA Bool Register Register Register Register
                    | MOV Bool Register Shifter
                    | MRS Bool Register
                    | MSR_immediate Bool (Immediate Word32) (Immediate Word32) (Immediate Int)
                    | MSR_register Bool (Immediate Word32) Register
                    | MUL Bool Register Register Register
                    | MVN Bool Register Shifter
                    | ORR Bool Register Register Shifter
                    | RSB Bool Register Register Shifter
                    | RSC Bool Register Register Shifter
                    | SBC Bool Register Register Shifter
                    | SMLAL Bool Register Register Register Register
                    | SMULL Bool Register Register Register Register
                    | STM1 AddressingMode4 [Register]
                    | STM2 AddressingMode4 [Register]
                    | STR Register AddressingMode2
                    | STRB Register AddressingMode2
                    | STRBT Register AddressingMode2
                    | STRH Register AddressingMode3
                    | STRT Register AddressingMode2
                    | SUB Bool Register Register Shifter
                    | TEQ Register Shifter
                    | TST Register Shifter
                    | UMLAL Bool Register Register Register Register
                    | UMULL Bool Register Register Register Register
  deriving (Show, Eq)
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
raw (MUL sFlag rd rm rs) = set rd (rm .* rs)
                   .>> (if sFlag then (
                          set nFlag (rd .|?| 31) .>>
                          set zFlag (rd .== 0)
                        ) else _id)
raw (MLA sFlag rd rm rs rn) = set rd ((rm .* rs) .+ rn)
                      .>> (if sFlag then (
                             set nFlag (rd .|?| 31) .>>
                             set zFlag (rd .== 0)
                           ) else _id)
raw (SMULL sFlag rdlo rdhi rm rs) =
      set rdhi (to32 .$ _bitRange 32 63 prod)
  .>> set rdlo (to32 .$ _bitRange  0 31 prod)
  .>> (if sFlag then (
         set nFlag (rdhi .|?| 31) .>>
         set zFlag ((rdhi .== 0) .&& (rdlo .== 0))
       ) else _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral (fromIntegral x :: Int32) :: Int64
        to32 x = fromIntegral x :: Word32
raw (UMULL sFlag rdlo rdhi rm rs) =
      set rdhi (to32 .$ _bitRange 32 63 prod)
  .>> set rdlo (to32 .$ _bitRange  0 31 prod)
  .>> (if sFlag then (
         set nFlag (rdhi .|?| 31) .>>
         set zFlag ((rdhi .== 0) .&& (rdlo .== 0))
       ) else _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral x :: Word64
        to32 x = fromIntegral x :: Word32
raw (SMLAL sFlag rdlo rdhi rm rs) =
      set rdhi ((to32 .$ _bitRange 32 63 prod) .+ rdhi .+ carry)
  .>> set rdlo ((to32 .$ _bitRange  0 31 prod) .+ rdlo)
  .>> (if sFlag then (
         set nFlag (rdhi .|?| 31) .>>
         set zFlag ((rdhi .== 0) .&& (rdlo .== 0))
       ) else _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral (fromIntegral x :: Int32) :: Int64
        to32 x = fromIntegral x :: Word32
        carry = _if (_carryFrom (to32 .$ _bitRange 0 31 prod) rdlo) % 1 ! 0
raw (UMLAL sFlag rdlo rdhi rm rs) =
      set rdhi ((to32 .$ _bitRange 32 63 prod) .+ rdhi .+ carry)
  .>> set rdlo ((to32 .$ _bitRange  0 31 prod) .+ rdlo)
  .>> (if sFlag then (
         set nFlag (rdhi .|?| 31) .>>
         set zFlag ((rdhi .== 0) .&& (rdlo .== 0))
       ) else _id)
  where prod = liftA2 (*) (to64 .$ rm) (to64 .$ rs)
        to64 x = fromIntegral x :: Word64
        to32 x = fromIntegral x :: Word32
        carry = _if (_carryFrom (to32 .$ _bitRange 0 31 prod) rdlo) % 1 ! 0
-- Status register access instructions
raw (MRS rFlag rd) = set rd psr
  where psr = if rFlag then spsr else cpsr
raw (MSR_immediate rFlag fieldMask immed8 rotateImm) = 
  if not rFlag then (
    let mask = _if (processorMode ./= pure User)
               %   (byteMask .& (userMask .| privMask))
               !   (byteMask .& userMask)
    in set cpsr ((cpsr .& (complement .$ mask)) .| (operand .& mask))
  ) else (
    let mask = byteMask .& (userMask .| privMask .| stateMask)
    in set spsr ((spsr .& (complement .$ mask)) .| (operand .& mask))
  )
  where operand = immed8 .@> (rotateImm .* 2)
        unallocMask = 0x0FFFFF00
        userMask = 0xF0000000
        privMask = 0x0000000F
        stateMask = 0x00000020
        byteMask = (_if (_testBit fieldMask 0) % 0x000000FF ! 0) .|
                   (_if (_testBit fieldMask 1) % 0x0000FF00 ! 0) .|
                   (_if (_testBit fieldMask 2) % 0x00FF0000 ! 0) .|
                   (_if (_testBit fieldMask 3) % 0xFF000000 ! 0)
raw (MSR_register rFlag fieldMask rm) = 
  if not rFlag then (
    let mask = _if (processorMode ./= pure User)
               %   (byteMask .& (userMask .| privMask))
               !   (byteMask .& userMask)
    in set cpsr ((cpsr .& (complement .$ mask)) .| (operand .& mask))
  ) else (
    let mask = byteMask .& (userMask .| privMask .| stateMask)
    in set spsr ((spsr .& (complement .$ mask)) .| (operand .& mask))
  )
  where operand = rm
        unallocMask = 0x0FFFFF00
        userMask = 0xF0000000
        privMask = 0x0000000F
        stateMask = 0x00000020
        byteMask = (_if (_testBit fieldMask 0) % 0x000000FF ! 0) .|
                   (_if (_testBit fieldMask 1) % 0x0000FF00 ! 0) .|
                   (_if (_testBit fieldMask 2) % 0x00FF0000 ! 0) .|
                   (_if (_testBit fieldMask 3) % 0xFF000000 ! 0)
-- Load and store instructions
raw (LDR rd am) = set rd (memory32 address)
              .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (LDRT rd am) = set rd (memory32 address)
               .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (LDRB rd am) = set rd (memory8 address)
               .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (LDRSB rd am) = set rd (signExtend 8 32 .$ memory8 address)
                .>> writeBack
  where (address, writeBack) = addressMode3 am
raw (LDRBT rd am) = set rd (memory8 address)
                .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (LDRH rd am) = set rd (memory16 address)
               .>> writeBack
  where (address, writeBack) = addressMode3 am
raw (LDRSH rd am) = set rd (signExtend 16 32 .$ memory16 address)
                .>> writeBack
  where (address, writeBack) = addressMode3 am
raw (STR rd am) = set (memory32 address) rd
              .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (STRT rd am) = set (memory32 address) rd
               .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (STRB rd am) = set (memory8 address) (bitRange 0 7 .$ rd)
               .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (STRBT rd am) = set (memory8 address) (bitRange 0 7 .$ rd)
                .>> writeBack
  where (address, writeBack) = addressMode2 am
raw (STRH rd am) = set (memory16 address) (bitRange 0 15 .$ rd)
               .>> writeBack
  where (address, writeBack) = addressMode3 am
-- Load and store multiple instructions
raw (LDM1 am registers) = fst $ foldr for (_id, 0) registers
  where (address, writeBack) = addressMode4 am registers
        for r (e, ii) = (e', ii .+ 4)
          where e' = e 
                 .>> set r (memory32 $ address .+ (4 .* ii))
-- TODO: LDM2 is incorrect. It needs to load to user registers
-- which is currently impossible with how the CPU is set up.
raw (LDM2 am registers) = fst $ foldr for (_id, 0) registers
  where (address, writeBack) = addressMode4 am registers
        for r (e, ii) = (e', ii .+ 4)
          where e' = e 
                 .>> set r (memory32 $ address .+ (4 .* ii))
raw (LDM3 am registers) = (fst $ foldr for (_id, 0) registers)
                         .>> set cpsr spsr
  where (address, writeBack) = addressMode4 am registers
        for r (e, ii) = (e', ii .+ 4)
          where e' = e 
                 .>> set r (memory32 $ address .+ (4 .* ii))
raw (STM1 am registers) = fst $ foldr for (_id, 0) registers
  where (address, writeBack) = addressMode4 am registers
        for r (e, ii) = (e', ii .+ 4)
          where e' = e 
                 .>> set (memory32 $ address .+ (4 .* ii)) r
-- TODO: STM2 is incorrect. It needs to store to user registers
-- which is currently impossible with how the CPU is set up.
raw (STM2 am registers) = fst $ foldr for (_id, 0) registers
  where (address, writeBack) = addressMode4 am registers
        for r (e, ii) = (e', ii .+ 4)
          where e' = e 
                 .>> set (memory32 $ address .+ (4 .* ii)) r
  
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

shifter (LSL_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % _pair rm cFlag
  !                       _pair (rm .<! shift_imm) (rm .|?| (32 - shift_imm))
}

shifter (LSL_R_operand rm rs) = do {
  _if  (rs' .== 0)  % _pair rm cFlag
  !_if (rs' .< 32)  % _pair (rm .<! rs') (rm .|?| (32 - rs'))
  !_if (rs' .== 32) % _pair 0 (rm .|?| 0)
  !                   _pair 0 (pure False)
} where rs' = fromIntegral .$ (bitRange 0 7) .$ rs

shifter (LSR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % _pair 0 (rm .|?| 31)
  !                       _pair (rm .!> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (LSR_R_operand rm rs) = do {
  _if  (rs' .== 0)  % _pair rm cFlag
  !_if (rs' .< 32)  % _pair (rm .!> rs') (rm .|?| (rs' - 1))
  !_if (rs' .== 32) % _pair 0 (rm .|?| 31)
  !                   _pair 0 (pure False)
} where rs' = fromIntegral .$ (bitRange 0 7) .$ rs

shifter (ASR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0) % (
    _if (not .$ (rm .|?| 31)) % _pair 0 (rm .|?| 31)
    !                           _pair 0xFFFFFFFF (rm .|?| 31)
  ) ! _pair (rm .?> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (ASR_R_operand rm rs) = do {
  _if (rs' .== 0)  % _pair rm cFlag
  !_if (rs' .< 32) % _pair (rm .?> rs') (rm .|?| (rs' - 1))
  ! ( _if (not .$ (rm .|?| 31)) % _pair 0 (rm .|?| 31)
      !                           _pair 0xFFFFFFFF (rm .|?| 31))
} where rs' = fromIntegral .$ bitRange 0 7 .$ rs

shifter (ROR_I_operand rm shift_imm) = do {
  _if (shift_imm .== 0)  % shifter (RRX_operand rm)
  !                        _pair (rm .@> shift_imm) (rm .|?| (shift_imm - 1))
}

shifter (ROR_R_operand rm rs) = do {
  _if (rs' .== 0)   % _pair rm cFlag
  !_if (rs'' .== 0) % _pair rm (rm .|?| 31)
  !                   _pair (rm .@> rs'') (rm .|?| (rs'' -1))
} where rs'  = fromIntegral .$ bitRange 0 7 .$ rs
        rs'' = fromIntegral .$ bitRange 0 4 .$ rs

shifter (RRX_operand rm) = do {
  _pair ((c .<! 31) .| (rm .!> 1)) (rm .|?| 0)
} where c = _if (cFlag) % 1 ! 0


dataProcess :: Bool -> Register -> Register -> Shifter
            -> (Register -> Immediate Word32 -> Immediate Word32)
            -> Flag -> Flag -> Flag -> Flag
            -> Execute
dataProcess sFlag rd rn s op nVal zVal cVal vVal =
  set rd (rn `op` shifterOperand s)
  .>> (if sFlag then (
          _if (rd .== r15)
          %   (set cpsr spsr)
          !   (set nFlag nVal
          .>>  set zFlag zVal
          .>>  set cFlag cVal
          .>>  set vFlag vVal)
       ) else _id)
