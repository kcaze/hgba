{-# LANGUAGE BinaryLiterals #-}
module Getters where

import Data.Bits
import Data.Word
import Bits

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

-- CPU Getters
type Get = CPU -> Word32

unbankedRegister  :: Get -> Get
fiqBankedRegister :: (Get, Get) -> Get
bankedRegister    :: (Get, Get, Get, Get, Get, Get) -> Get

unbankedRegister = id
fiqBankedRegister (r, r_fiq) =
  do mode <- processorMode
     case mode of
       FIQ -> r_fiq
       _   -> r
bankedRegister (r, r_abt, r_fiq, r_irq, r_svc, r_und) =
  do mode <- processorMode
     case mode of
       Abort        -> r_abt
       FIQ          -> r_fiq
       IRQ          -> r_irq
       Supervisor   -> r_svc
       Undefined    -> r_und
       _            -> r

[r0, r1, r2, r3, r4, r5, r6, r7] = map unbankedRegister rs
  where rs = [cpu_r0, cpu_r1, cpu_r2, cpu_r3, cpu_r4, cpu_r5, cpu_r6, cpu_r7]
[r8, r9, r10, r11, r12] = map fiqBankedRegister rs
  where rs = [(cpu_r8, cpu_r8_fiq), (cpu_r9, cpu_r9_fiq),
              (cpu_r10, cpu_r10_fiq), (cpu_r11, cpu_r11_fiq),
              (cpu_r12, cpu_r12_fiq)]
[r13, r14] = map bankedRegister rs
  where rs = [(cpu_r13, cpu_r13_abt, cpu_r13_fiq,
               cpu_r13_irq, cpu_r13_svc, cpu_r13_und),
              (cpu_r14, cpu_r14_abt, cpu_r14_fiq,
               cpu_r14_irq, cpu_r14_svc, cpu_r14_und)]
r15 = unbankedRegister cpu_r15
[sp, lr, pc] = [r13, r14, r15]

register :: Int -> Get
register n = rs !! n
  where rs = [r0, r1, r2, r3, r4, r5, r6, r7,
              r8, r9, r10, r11, r12, r13, r14, r15]

cpsr :: Get
cpsr = cpu_cpsr

spsr :: Get
spsr = do mode <- processorMode
          case mode of
            Abort      -> cpu_spsr_abt
            FIQ        -> cpu_spsr_fiq
            IRQ        -> cpu_spsr_irq
            Supervisor -> cpu_spsr_svc
            Undefined  -> cpu_spsr_und
            _          -> undefined

[nBit, zBit, cBit, vBit, iBit, fBit, tBit] = map getBit bits
  where bits = [31, 30, 29, 28, 7, 6, 5]
        getBit b = ((/= 0) . bitRange b b) <$> cpsr

processorMode :: CPU -> ProcessorMode
processorMode =
  do mode <- bitRange 0 4 <$> cpsr
     case mode of
       0b01000 -> return User
       0b10001 -> return FIQ
       0b10010 -> return IRQ
       0b10011 -> return Supervisor
       0b10111 -> return Abort
       0b11011 -> return Undefined
       0b11111 -> return System
       _ -> undefined

-- CPU Setters
setR0 = setRegister 0
setR1 = setRegister 1
setR2 = setRegister 2
setR3 = setRegister 3
setR4 = setRegister 4
setR5 = setRegister 5
setR6 = setRegister 6
setR7 = setRegister 7
setR8 = setRegister 8
setR9 = setRegister 9
setR10 = setRegister 10
setR11 = setRegister 11
setR12 = setRegister 12
setR13 = setRegister 13
setR14 = setRegister 14
setR15 = setRegister 15
setRegister :: Int -> (CPU -> Word32) -> (CPU -> CPU)
setRegister 0 f = do { x <- f; \c -> c { cpu_r0 = x } }
setRegister 1 f = do { x <- f; \c -> c { cpu_r1 = x } }
setRegister 2 f = do { x <- f; \c -> c { cpu_r2 = x } }
setRegister 3 f = do { x <- f; \c -> c { cpu_r3 = x } }
setRegister 4 f = do { x <- f; \c -> c { cpu_r4 = x } }
setRegister 5 f = do { x <- f; \c -> c { cpu_r5 = x } }
setRegister 6 f = do { x <- f; \c -> c { cpu_r6 = x } }
setRegister 7 f = do { x <- f; \c -> c { cpu_r7 = x } }
setRegister 8 f =
  do x <- f
     mode <- processorMode
     case mode of
       FIQ -> \c -> c { cpu_r8_fiq = x }
       _   -> \c -> c { cpu_r8 = x }
setRegister 9 f =
  do x <- f
     mode <- processorMode
     case mode of
       FIQ -> \c -> c { cpu_r9_fiq = x }
       _   -> \c -> c { cpu_r9 = x }
setRegister 10 f =
  do x <- f
     mode <- processorMode
     case mode of
       FIQ -> \c -> c { cpu_r10_fiq = x }
       _   -> \c -> c { cpu_r10 = x }
setRegister 11 f =
  do x <- f
     mode <- processorMode
     case mode of
       FIQ -> \c -> c { cpu_r11_fiq = x }
       _   -> \c -> c { cpu_r11 = x }
setRegister 12 f =
  do x <- f
     mode <- processorMode
     case mode of
       FIQ -> \c -> c { cpu_r12_fiq = x }
       _   -> \c -> c { cpu_r12 = x }
setRegister 13 f =
  do x <- f
     mode <- processorMode
     case mode of
       Abort      -> \c -> c { cpu_r13_abt = x }
       FIQ        -> \c -> c { cpu_r13_fiq = x }
       IRQ        -> \c -> c { cpu_r13_irq = x }
       Supervisor -> \c -> c { cpu_r13_svc = x }
       Undefined  -> \c -> c { cpu_r13_und = x }
       _          -> \c -> c { cpu_r13 = x }
setRegister 14 f =
  do x <- f
     mode <- processorMode
     case mode of
       Abort      -> \c -> c { cpu_r14_abt = x }
       FIQ        -> \c -> c { cpu_r14_fiq = x }
       IRQ        -> \c -> c { cpu_r14_irq = x }
       Supervisor -> \c -> c { cpu_r14_svc = x }
       Undefined  -> \c -> c { cpu_r14_und = x }
       _          -> \c -> c { cpu_r14 = x }
setRegister 15 f = do { x <- f; \c -> c { cpu_r15 = x } }

setCPSR f = do { x <- f; \c -> c { cpu_cpsr = x } }
setSPSR f =
  do x <- f
     mode <- processorMode
     case mode of
       Abort      -> \c -> c { cpu_spsr_abt = x }
       FIQ        -> \c -> c { cpu_spsr_fiq = x }
       IRQ        -> \c -> c { cpu_spsr_irq = x }
       Supervisor -> \c -> c { cpu_spsr_svc = x }
       Undefined  -> \c -> c { cpu_spsr_und = x }
       _ -> undefined

[setNBit, setZBit, setCBit, setVBit, setIBit, setFBit, setTBit] = map set bits
  where bits = [31, 30, 29, 28, 7, 6, 5]
        set b f =
          do fn <- (\cond -> if cond then setBit else clearBit) <$> f
             w <- cpsr
             \c -> c { cpu_cpsr = fn w b }

setProcessorMode :: (CPU -> ProcessorMode) -> (CPU -> CPU)
setProcessorMode f =
  do mode <- f
     w <- cpsr
     let modeBits = case mode of
                      User       -> 0b10000
                      FIQ        -> 0b10001
                      IRQ        -> 0b10010
                      Supervisor -> 0b10011
                      Abort      -> 0b10111
                      Undefined  -> 0b11011
                      System     -> 0b11111
     \c -> c { cpu_cpsr = (w .&. complement 0x1F) .|. modeBits }
