{-# LANGUAGE BinaryLiterals #-}
module CPU ( r0, r1, r2, r3, r4, r5, r6, r7, r8, r9
           , r10, r11, r12, r13, r14, r15, sp, lr, pc
           , register
           , cpsr, spsr
           , nFlag, zFlag, cFlag, vFlag, iFlag, fFlag, tFlag
           , nBit, zBit, cBit, vBit, iBit, fBit, tBit
           , processorMode
           , memory8, memory16, memory32
           , instructionSize
           , fromBit, fromFlag
           ) where

import Data.Bits
import Numeric
import Data.Word

import Imperative
import Memory
import Types

-- Utility functions
fromBit :: Bit -> Flag
fromFlag :: Flag -> Bit

fromBit (Immutable g) = Immutable ((/= 0) . g)
fromBit (Mutable g s) = Mutable ((/= 0) . g) (s . (\x -> if x then 1 else 0))
fromFlag (Immutable g) = Immutable ((\x -> if x then 1 else 0) . g)
fromFlag (Mutable g s) = Mutable ((\x -> if x then 1 else 0) . g) (s . (/= 0))

-- Pure CPU Getters
type GetW = CPU -> Word32
type GetB = CPU -> Bool

getUnbankedRegister  :: GetW -> GetW
getFIQBankedRegister :: (GetW, GetW) -> GetW
getBankedRegister    :: (GetW, GetW, GetW, GetW, GetW, GetW) -> GetW

getUnbankedRegister = id
getFIQBankedRegister (r, r_fiq) = _if (getProcessorMode .== pure FIQ) % r_fiq ! r
getBankedRegister (r, r_abt, r_fiq, r_irq, r_svc, r_und) =
    _if (getProcessorMode .== pure Abort)      % r_abt
  ! _if (getProcessorMode .== pure FIQ)        % r_fiq
  ! _if (getProcessorMode .== pure IRQ)        % r_irq
  ! _if (getProcessorMode .== pure Supervisor) % r_svc
  ! _if (getProcessorMode .== pure Undefined)  % r_und
  ! r

[getR0, getR1, getR2, getR3, getR4, getR5, getR6, getR7] = map getUnbankedRegister rs
  where rs = [cpu_r0, cpu_r1, cpu_r2, cpu_r3, cpu_r4, cpu_r5, cpu_r6, cpu_r7]
[getR8, getR9, getR10, getR11, getR12] = map getFIQBankedRegister rs
  where rs = [(cpu_r8, cpu_r8_fiq), (cpu_r9, cpu_r9_fiq),
              (cpu_r10, cpu_r10_fiq), (cpu_r11, cpu_r11_fiq),
              (cpu_r12, cpu_r12_fiq)]
[getR13, getR14] = map getBankedRegister rs
  where rs = [(cpu_r13, cpu_r13_abt, cpu_r13_fiq,
               cpu_r13_irq, cpu_r13_svc, cpu_r13_und),
              (cpu_r14, cpu_r14_abt, cpu_r14_fiq,
               cpu_r14_irq, cpu_r14_svc, cpu_r14_und)]
getR15 = getUnbankedRegister cpu_r15

getCPSR :: CPU -> Word32
getCPSR = cpu_cpsr

getSPSR :: CPU -> Word32
getSPSR = _if (getProcessorMode .== pure Abort)      % cpu_spsr_abt
        ! _if (getProcessorMode .== pure FIQ)        % cpu_spsr_fiq
        ! _if (getProcessorMode .== pure IRQ)        % cpu_spsr_irq
        ! _if (getProcessorMode .== pure Supervisor) % cpu_spsr_svc
        ! _if (getProcessorMode .== pure Undefined)  % cpu_spsr_und
        ! 0 --error "Attempt to get SPSR in user or system mode." 

[getNFlag, getZFlag, getCFlag, getVFlag, getIFlag, getFFlag, getTFlag] = map getBit bits
  where bits = [31, 30, 29, 28, 7, 6, 5]
        getBit b c = bitRange b b (getCPSR c) /= 0

getProcessorMode :: CPU -> ProcessorMode
getProcessorMode cpu = if (mode == 0b01000) then User
                  else if (mode == 0b10001) then FIQ
                  else if (mode == 0b10010) then IRQ
                  else if (mode == 0b10011) then Supervisor
                  else if (mode == 0b10111) then Abort
                  else if (mode == 0b11011) then Undefined
                  else if (mode == 0b11111) then System
                  else error $ "CPU has invalid processor mode 0x" ++ showHex mode ""
  where mode = get cpsr cpu .&. 0x1F

getMemory8 :: Address -> CPU -> Word32
getMemory16 :: Address -> CPU -> Word32
getMemory32 :: Address -> CPU -> Word32
getMemory8 a cpu = read8 a (cpu_memory cpu) 
getMemory16 a cpu = read16 a (cpu_memory cpu) 
getMemory32 a cpu = read32 a (cpu_memory cpu) 

-- Pure CPU Setters
setR0 x c = c { cpu_r0 = x }
setR1 x c = c { cpu_r1 = x }
setR2 x c = c { cpu_r2 = x }
setR3 x c = c { cpu_r3 = x }
setR4 x c = c { cpu_r4 = x }
setR5 x c = c { cpu_r5 = x }
setR6 x c = c { cpu_r6 = x }
setR7 x c = c { cpu_r7 = x }
setR8 x c = case mode of FIQ -> c { cpu_r8_fiq = x }
                         _   -> c { cpu_r8 = x }
  where mode = getProcessorMode c
setR9 x c = case mode of FIQ -> c { cpu_r9_fiq = x }
                         _   -> c { cpu_r9 = x }
  where mode = getProcessorMode c
setR10 x c = case mode of FIQ -> c { cpu_r10_fiq = x }
                          _   -> c { cpu_r10 = x }
  where mode = getProcessorMode c
setR11 x c = case mode of FIQ -> c { cpu_r11_fiq = x }
                          _   -> c { cpu_r11 = x }
  where mode = getProcessorMode c
setR12 x c = case mode of FIQ -> c { cpu_r12_fiq = x }
                          _   -> c { cpu_r12 = x }
  where mode = getProcessorMode c
setR13 x c = case mode of Abort      -> c { cpu_r13_abt = x }
                          FIQ        -> c { cpu_r13_fiq = x }
                          IRQ        -> c { cpu_r13_irq = x }
                          Supervisor -> c { cpu_r13_svc = x }
                          Undefined  -> c { cpu_r13_und = x }
                          _          -> c { cpu_r13 = x }
  where mode = getProcessorMode c
setR14 x c = case mode of Abort      -> c { cpu_r14_abt = x }
                          FIQ        -> c { cpu_r14_fiq = x }
                          IRQ        -> c { cpu_r14_irq = x }
                          Supervisor -> c { cpu_r14_svc = x }
                          Undefined  -> c { cpu_r14_und = x }
                          _          -> c { cpu_r14 = x }
  where mode = getProcessorMode c
-- Setting r15 flushes pipeline
-- Also, the value in r15 is always word / halfword aligned depending on
-- the state. 
setR15 x c = c {
               cpu_r15 = x .&. (if getTFlag c then 0xFFFFFFFE else 0xFFFFFFFC),
               cpu_fetch = Nothing,
               cpu_decode = Nothing
             }

setCPSR x c = c { cpu_cpsr = x }

setSPSR x c = 
  case mode of Abort      -> c { cpu_spsr_abt = x }
               FIQ        -> c { cpu_spsr_abt = x }
               IRQ        -> c { cpu_spsr_abt = x }
               Supervisor -> c { cpu_spsr_abt = x }
               Undefined  -> c { cpu_spsr_abt = x }
               _          -> error "Attempt to set SPSR in user or system mode."
  where mode = getProcessorMode c

[setNFlag, setZFlag, setCFlag, setVFlag, setIFlag, setFFlag, setTFlag] = map set bits
  where bits = [31, 30, 29, 28, 7, 6, 5]
        set b v c = c { cpu_cpsr = fn (getCPSR c) b }
          where fn = if v then setBit else clearBit

setProcessorMode m c = c { cpu_cpsr = ((getCPSR c) .&. complement 0x1F) .|. b }
  where b = case m of User       -> 0b10000
                      FIQ        -> 0b10001
                      IRQ        -> 0b10010
                      Supervisor -> 0b10011
                      Abort      -> 0b10111
                      Undefined  -> 0b11011
                      System     -> 0b11111

setMemory8 :: Address -> Word32 -> CPU -> CPU
setMemory16 :: Address -> Word32 -> CPU -> CPU
setMemory32 :: Address -> Word32 -> CPU -> CPU
setMemory8 a w cpu = cpu { cpu_memory = write8 a w (cpu_memory cpu) }
setMemory16 a w cpu = cpu { cpu_memory = write16 a w (cpu_memory cpu) }
setMemory32 a w cpu = cpu { cpu_memory = write32 a w (cpu_memory cpu) }

-- Mutable CPU values
[r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15,
 sp, lr, pc] = map (uncurry $ uncurry Register) rs :: [Register]
  where rs = [((0, getR0), setR0), ((1, getR1), setR1), ((2, getR2), setR2),
              ((3, getR3), setR3), ((4, getR4), setR4), ((5, getR5), setR5),
              ((6, getR6), setR6), ((7, getR7), setR7), ((8, getR8), setR8),
              ((9, getR9), setR9), ((10, getR10), setR10), ((11, getR11), setR11),
              ((12, getR12), setR12), ((13, getR13), setR13), ((14, getR14), setR14),
              ((15, getR15), setR15), ((13, getR13), setR13), ((14, getR14), setR14),
              ((15, getR15), setR15)]

register :: Int -> Register
register 0 = r0
register 1 = r1
register 2 = r2
register 3 = r3
register 4 = r4
register 5 = r5
register 6 = r6
register 7 = r7
register 8 = r8
register 9 = r9
register 10 = r10
register 11 = r11
register 12 = r12
register 13 = r13
register 14 = r14
register 15 = r15

cpsr = Mutable getCPSR setCPSR
spsr = Mutable getSPSR setSPSR

[nFlag, zFlag, cFlag, vFlag, iFlag, fFlag, tFlag] = map (uncurry Mutable) bs :: [Flag]
  where bs = [(getNFlag, setNFlag), (getZFlag, setZFlag), (getCFlag, setCFlag),
              (getVFlag, setVFlag), (getIFlag, setIFlag), (getFFlag, setFFlag),
              (getTFlag, setTFlag)]
[nBit, zBit, cBit, vBit, iBit, fBit, tBit] = map fromFlag
  [nFlag, zFlag, cFlag, vFlag, iFlag, fFlag, tFlag]
processorMode = Mutable getProcessorMode setProcessorMode
memory8 :: Value CPU Address -> Value CPU Word32
memory16 :: Value CPU Address -> Value CPU Word32
memory32 :: Value CPU Address -> Value CPU Word32
memory8 address = Mutable (\c -> getMemory8 (get address c) c)
                          (\w c -> setMemory8 (get address c) w c) 
memory16 address = Mutable (\c -> getMemory16 (get address c) c)
                          (\w c -> setMemory16 (get address c) w c) 
memory32 address = Mutable (\c -> getMemory32 (get address c) c)
                          (\w c -> setMemory32 (get address c) w c)

-- Immutable CPU values (i.e. impure functions)
instructionSize :: Immediate Word32
instructionSize = _if tFlag % 2 ! 4

-- Show instance for CPU
instance Show CPU where
  show c = "CPU {\n"
        ++ "          r0 = " ++ showHex (getR0 c) "\n"
        ++ "          r1 = " ++ showHex (getR1 c) "\n"
        ++ "          r2 = " ++ showHex (getR2 c) "\n"
        ++ "          r3 = " ++ showHex (getR3 c) "\n"
        ++ "          r4 = " ++ showHex (getR4 c) "\n"
        ++ "          r5 = " ++ showHex (getR5 c) "\n"
        ++ "          r6 = " ++ showHex (getR6 c) "\n"
        ++ "          r7 = " ++ showHex (getR7 c) "\n"
        ++ "          r8 = " ++ showHex (getR8 c) "\n"
        ++ "          r9 = " ++ showHex (getR9 c) "\n"
        ++ "         r10 = " ++ showHex (getR10 c) "\n"
        ++ "         r11 = " ++ showHex (getR11 c) "\n"
        ++ "         r12 = " ++ showHex (getR12 c) "\n"
        ++ "         r13 = " ++ showHex (getR13 c) "\n"
        ++ "         r14 = " ++ showHex (getR14 c) "\n"
        ++ "         r15 = " ++ showHex (getR15 c) "\n"
        ++ "        cpsr = " ++ showHex (getCPSR c) "\n"
        ++ "        spsr = " ++ showHex (getSPSR c) "\n"
        ++ "           n = " ++ show (getNFlag c) ++ "\n"
        ++ "           z = " ++ show (getZFlag c) ++ "\n"
        ++ "           c = " ++ show (getCFlag c) ++ "\n"
        ++ "           v = " ++ show (getVFlag c) ++ "\n"
        ++ "           i = " ++ show (getIFlag c) ++ "\n"
        ++ "           f = " ++ show (getFFlag c) ++ "\n"
        ++ "           t = " ++ show (getTFlag c) ++ "\n"
        ++ "        mode = " ++ show (getProcessorMode c) ++ "\n"
        ++ "       fetch = " ++ show (cpu_fetch c) ++ "\n"
        ++ "      decode = " ++ show (cpu_decode c) ++ "\n"
        ++ "      cycles = " ++ show (cpu_cycles c) ++ "\n"
        ++ "}"

-- Detailed show.
show' c = "CPU {\n"
        ++ "          r0 = " ++ showHex (cpu_r0 c) "\n"
        ++ "          r1 = " ++ showHex (cpu_r1 c) "\n"
        ++ "          r2 = " ++ showHex (cpu_r2 c) "\n"
        ++ "          r3 = " ++ showHex (cpu_r3 c) "\n"
        ++ "          r4 = " ++ showHex (cpu_r4 c) "\n"
        ++ "          r5 = " ++ showHex (cpu_r5 c) "\n"
        ++ "          r6 = " ++ showHex (cpu_r6 c) "\n"
        ++ "          r7 = " ++ showHex (cpu_r7 c) "\n"
        ++ "          r8 = " ++ showHex (cpu_r8 c) "\n"
        ++ "          r9 = " ++ showHex (cpu_r9 c) "\n"
        ++ "         r10 = " ++ showHex (cpu_r10 c) "\n"
        ++ "         r11 = " ++ showHex (cpu_r11 c) "\n"
        ++ "         r12 = " ++ showHex (cpu_r12 c) "\n"
        ++ "         r13 = " ++ showHex (cpu_r13 c) "\n"
        ++ "         r14 = " ++ showHex (cpu_r14 c) "\n"
        ++ "         r15 = " ++ showHex (cpu_r15 c) "\n"
        ++ "           n = " ++ show (getNFlag c) ++ "\n"
        ++ "           z = " ++ show (getZFlag c) ++ "\n"
        ++ "           c = " ++ show (getCFlag c) ++ "\n"
        ++ "           v = " ++ show (getVFlag c) ++ "\n"
        ++ "           i = " ++ show (getIFlag c) ++ "\n"
        ++ "           f = " ++ show (getFFlag c) ++ "\n"
        ++ "           t = " ++ show (getTFlag c) ++ "\n"
        ++ "        mode = " ++ show (getProcessorMode c) ++ "\n"
        ++ "      r8_fiq = " ++ showHex (cpu_r8_fiq c) "\n"
        ++ "      r9_fiq = " ++ showHex (cpu_r9_fiq c) "\n"
        ++ "     r10_fiq = " ++ showHex (cpu_r10_fiq c) "\n"
        ++ "     r11_fiq = " ++ showHex (cpu_r11_fiq c) "\n"
        ++ "     r12_fiq = " ++ showHex (cpu_r12_fiq c) "\n"
        ++ "     r13_abt = " ++ showHex (cpu_r13_abt c) "\n"
        ++ "     r13_fiq = " ++ showHex (cpu_r13_fiq c) "\n"
        ++ "     r13_irq = " ++ showHex (cpu_r13_irq c) "\n"
        ++ "     r13_svc = " ++ showHex (cpu_r13_svc c) "\n"
        ++ "     r13_und = " ++ showHex (cpu_r13_und c) "\n"
        ++ "     r14_abt = " ++ showHex (cpu_r14_abt c) "\n"
        ++ "     r14_fiq = " ++ showHex (cpu_r14_fiq c) "\n"
        ++ "     r14_irq = " ++ showHex (cpu_r14_irq c) "\n"
        ++ "     r14_svc = " ++ showHex (cpu_r14_svc c) "\n"
        ++ "     r14_und = " ++ showHex (cpu_r14_und c) "\n"
        ++ "        cpsr = " ++ showHex (cpu_cpsr c) "\n"
        ++ "    spsr_abt = " ++ showHex (cpu_spsr_abt c) "\n"
        ++ "    spsr_fiq = " ++ showHex (cpu_spsr_fiq c) "\n"
        ++ "    spsr_irq = " ++ showHex (cpu_spsr_irq c) "\n"
        ++ "    spsr_svc = " ++ showHex (cpu_spsr_svc c) "\n"
        ++ "    spsr_und = " ++ showHex (cpu_spsr_und c) "\n"
        ++ "       fetch = " ++ show (cpu_fetch c) ++ "\n"
        ++ "      decode = " ++ show (cpu_decode c) ++ "\n"
        ++ "      cycles = " ++ show (cpu_cycles c) ++ "\n"
        ++ "}"
