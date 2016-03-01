{-# LANGUAGE BinaryLiterals #-}
module CPU ( ProcessorMode(..)
           , Immediate
           , Register
           , Flag
           , Execute
           , r0, r1, r2, r3, r4, r5, r6, r7, r8, r9
           , r10, r11, r12, r13, r14, r15, sp, lr, pc
           , cpsr, spsr
           , nFlag, zFlag, cFlag, vFlag, iFlag, fFlag, tFlag
           , nBit, zBit, cBit, vBit, iBit, fBit, tBit
           , processorMode
           , instructionSize
           , execute, fromBit, fromFlag
           ) where

import Data.Bits
import Data.Word
import Bits


-- Utility functions
execute :: Execute -> CPU -> CPU
fromBit :: Bit -> Flag
fromFlag :: Flag -> Bit

execute = get
fromBit (Immutable g) = Immutable ((/= 0) . g)
fromBit (Mutable g s) = Mutable ((/= 0) . g) (s . (\x -> if x then 1 else 0))
fromFlag (Immutable g) = Immutable ((\x -> if x then 1 else 0) . g)
fromFlag (Mutable g s) = Mutable ((\x -> if x then 1 else 0) . g) (s . (/= 0))

-- Data types
type Immediate a = Value CPU a 
type Register = Value CPU Word32
type Flag = Value CPU Bool
type Bit = Value CPU Word32
type Execute = Value CPU CPU

data ProcessorMode = User
                   | FIQ
                   | IRQ
                   | Supervisor
                   | Abort
                   | Undefined
                   | System
  deriving (Eq, Show)

data CPU = CPU {
  cpu_r0        :: Word32,
  cpu_r1        :: Word32,
  cpu_r2        :: Word32,
  cpu_r3        :: Word32,
  cpu_r4        :: Word32,
  cpu_r5        :: Word32,
  cpu_r6        :: Word32,
  cpu_r7        :: Word32,
  cpu_r8        :: Word32,
  cpu_r8_fiq    :: Word32,
  cpu_r9        :: Word32,
  cpu_r9_fiq    :: Word32,
  cpu_r10       :: Word32,
  cpu_r10_fiq   :: Word32,
  cpu_r11       :: Word32,
  cpu_r11_fiq   :: Word32,
  cpu_r12       :: Word32,
  cpu_r12_fiq   :: Word32,
  cpu_r13       :: Word32,
  cpu_r13_abt   :: Word32,
  cpu_r13_fiq   :: Word32,
  cpu_r13_irq   :: Word32,
  cpu_r13_svc   :: Word32,
  cpu_r13_und   :: Word32,
  cpu_r14       :: Word32,
  cpu_r14_abt   :: Word32,
  cpu_r14_fiq   :: Word32,
  cpu_r14_irq   :: Word32,
  cpu_r14_svc   :: Word32,
  cpu_r14_und   :: Word32,
  cpu_r15       :: Word32,
  cpu_cpsr      :: Word32,
  cpu_spsr_abt  :: Word32,
  cpu_spsr_fiq  :: Word32,
  cpu_spsr_irq  :: Word32,
  cpu_spsr_svc  :: Word32,
  cpu_spsr_und  :: Word32
} deriving (Eq, Show)

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
        ! error "Attempt to get SPSR in user or system mode." 

[getNFlag, getZFlag, getCFlag, getVFlag, getIFlag, getFFlag, getTFlag] = map getBit bits
  where bits = [31, 30, 29, 28, 7, 6, 5]
        getBit b c = bitRange b b (getCPSR c) /= 0

getProcessorMode :: CPU -> ProcessorMode
getProcessorMode = _if (mode .== 0b01000) % pure User
                 ! _if (mode .== 0b10001) % pure FIQ
                 ! _if (mode .== 0b10010) % pure IRQ
                 ! _if (mode .== 0b10011) % pure Supervisor
                 ! _if (mode .== 0b10111) % pure Abort
                 ! _if (mode .== 0b11011) % pure Undefined
                 ! _if (mode .== 0b11111) % pure System
                 ! error "CPU has invalid processor mode."
  where mode = getCPSR .& 0x1F

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
setR15 x c = c { cpu_r15 = x }

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

-- Mutable CPU values
[r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15,
 sp, lr, pc, cpsr, spsr] = map (uncurry Mutable) rs :: [Register]
  where rs = [(getR0, setR0), (getR1, setR1), (getR2, setR2), (getR3, setR3),
         (getR4, setR4), (getR5, setR5), (getR6, setR6), (getR7, setR7),
         (getR8, setR8), (getR9, setR9), (getR10, setR10), (getR11, setR11),
         (getR12, setR12), (getR13, setR13), (getR14, setR14), (getR15, setR15),
         (getR13, setR13), (getR14, setR14), (getR15, setR15), (getCPSR, setCPSR),
         (getSPSR, setSPSR)]
[nFlag, zFlag, cFlag, vFlag, iFlag, fFlag, tFlag] = map (uncurry Mutable) bs :: [Flag]
  where bs = [(getNFlag, setNFlag), (getZFlag, setZFlag), (getCFlag, setCFlag),
              (getVFlag, setVFlag), (getIFlag, setIFlag), (getFFlag, setFFlag),
              (getTFlag, setTFlag)]
[nBit, zBit, cBit, vBit, iBit, fBit, tBit] = map fromFlag
  [nFlag, zFlag, cFlag, vFlag, iFlag, fFlag, tFlag]
processorMode = Mutable getProcessorMode setProcessorMode

-- Immutable CPU values (i.e. impure functions
instructionSize :: Immediate Word32
instructionSize = _if tFlag % 4 ! 8

-- CPU constructor
reset :: Execute
reset = setsvc
    .>> set processorMode (pure Supervisor)
    .>> set iFlag (pure True)
    .>> set fFlag (pure True)
    .>> set pc 0
  where setsvc = fromFunction (\cpu -> cpu { cpu_r14_svc = getR15 cpu, 
                                             cpu_spsr_svc = getCPSR cpu })

powerUp :: CPU
powerUp = execute reset CPU {
  cpu_r0        = 0,
  cpu_r1        = 0,
  cpu_r2        = 0,
  cpu_r3        = 0,
  cpu_r4        = 0,
  cpu_r5        = 0,
  cpu_r6        = 0,
  cpu_r7        = 0,
  cpu_r8        = 0,
  cpu_r8_fiq    = 0,
  cpu_r9        = 0,
  cpu_r9_fiq    = 0,
  cpu_r10       = 0,
  cpu_r10_fiq   = 0,
  cpu_r11       = 0,
  cpu_r11_fiq   = 0,
  cpu_r12       = 0,
  cpu_r12_fiq   = 0,
  cpu_r13       = 0,
  cpu_r13_abt   = 0,
  cpu_r13_fiq   = 0,
  cpu_r13_irq   = 0,
  cpu_r13_svc   = 0,
  cpu_r13_und   = 0,
  cpu_r14       = 0,
  cpu_r14_abt   = 0,
  cpu_r14_fiq   = 0,
  cpu_r14_irq   = 0,
  cpu_r14_svc   = 0,
  cpu_r14_und   = 0,
  cpu_r15       = 0,
  cpu_cpsr      = 0,
  cpu_spsr_abt  = 0,
  cpu_spsr_fiq  = 0,
  cpu_spsr_irq  = 0,
  cpu_spsr_svc  = 0,
  cpu_spsr_und  = 0
}
