module Register ( getR0, getR1, getR2, getR3
                , getR4, getR5, getR6, getR7
                , getR8, getR9, getR10, getR11
                , getR12, getR13, getR14, getR15
                , setR0, setR1, setR2, setR3
                , setR4, setR5, setR6, setR7
                , setR8, setR9, setR10, setR11
                , setR12, setR13, setR14, setR15
                , getRegister
                , setRegister
                , getCPSR
                , setCPSR
                , getSPSR
                , setSPSR
                , getConditionCodeFlags
                , setConditionCodeFlags
                , getProcessorMode
                , setProcessorMode
                , getThumbStateFlag
                , setThumbStateFlag
                , getNFlag
                , setNFlag
                , getZFlag
                , setZFlag
                , getCFlag
                , setCFlag
                , getVFlag
                , setVFlag
                , statusRegisterToWord32
                , word32ToStatusRegister
                )
                where

import Data.Word
import Data.Bits
import Types

-- General purpose register getters and setters.
getRegister :: CPU -> Register -> Word32
setRegister :: CPU -> Register -> Word32 -> CPU

getR0 = flip getRegister 0
getR1 = flip getRegister 1
getR2 = flip getRegister 2
getR3 = flip getRegister 3
getR4 = flip getRegister 4
getR5 = flip getRegister 5
getR6 = flip getRegister 6
getR7 = flip getRegister 7
getR8 = flip getRegister 8
getR9 = flip getRegister 9
getR10 = flip getRegister 10
getR11 = flip getRegister 11
getR12 = flip getRegister 12
getR13 = flip getRegister 13
getR14 = flip getRegister 14
getR15 = flip getRegister 15

setR0 = flip setRegister 0
setR1 = flip setRegister 1
setR2 = flip setRegister 2
setR3 = flip setRegister 3
setR4 = flip setRegister 4
setR5 = flip setRegister 5
setR6 = flip setRegister 6
setR7 = flip setRegister 7
setR8 = flip setRegister 8
setR9 = flip setRegister 9
setR10 = flip setRegister 10
setR11 = flip setRegister 11
setR12 = flip setRegister 12
setR13 = flip setRegister 13
setR14 = flip setRegister 14
setR15 = flip setRegister 15

getRegister cpu 0 = r0 . registers $ cpu
getRegister cpu 1 = r1 . registers $ cpu
getRegister cpu 2 = r2 . registers $ cpu
getRegister cpu 3 = r3 . registers $ cpu
getRegister cpu 4 = r4 . registers $ cpu
getRegister cpu 5 = r5 . registers $ cpu
getRegister cpu 6 = r6 . registers $ cpu
getRegister cpu 7 = r7 . registers $ cpu
getRegister cpu 8 = (case (getProcessorMode cpu) of
  FIQ -> r8_fiq . registers
  _   -> r8 . registers) $ cpu
getRegister cpu 9 = (case (getProcessorMode cpu) of
  FIQ -> r9_fiq . registers
  _   -> r9 . registers) $ cpu
getRegister cpu 10 = (case (getProcessorMode cpu) of
  FIQ -> r10_fiq . registers
  _   -> r10 . registers) $ cpu
getRegister cpu 11 = (case (getProcessorMode cpu) of
  FIQ -> r11_fiq . registers
  _   -> r11 . registers) $ cpu
getRegister cpu 12 = (case (getProcessorMode cpu) of
  FIQ -> r12_fiq . registers
  _   -> r12 . registers) $ cpu
getRegister cpu 13 = (case (getProcessorMode cpu) of
  User -> r13 . registers
  FIQ -> r13_fiq . registers
  IRQ -> r13_irq . registers
  Supervisor -> r13_svc . registers
  Abort -> r13_abt . registers
  Undefined -> r13_und . registers
  System -> r13 . registers) $ cpu
getRegister cpu 14 = (case (getProcessorMode cpu) of
  User -> r14 . registers
  FIQ -> r14_fiq . registers
  IRQ -> r14_irq . registers
  Supervisor -> r14_svc . registers
  Abort -> r14_abt . registers
  Undefined -> r14_und . registers
  System -> r14 . registers) $ cpu
getRegister cpu 15 = pc . registers $ cpu

setRegister cpu 0 x = cpu { registers = (registers cpu) { r0 = x } }
setRegister cpu 1 x = cpu { registers = (registers cpu) { r1 = x } }
setRegister cpu 2 x = cpu { registers = (registers cpu) { r2 = x } }
setRegister cpu 3 x = cpu { registers = (registers cpu) { r3 = x } }
setRegister cpu 4 x = cpu { registers = (registers cpu) { r4 = x } }
setRegister cpu 5 x = cpu { registers = (registers cpu) { r5 = x } }
setRegister cpu 6 x = cpu { registers = (registers cpu) { r6 = x } }
setRegister cpu 7 x = cpu { registers = (registers cpu) { r7 = x } }
setRegister cpu 8 x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r8_fiq = x }
                      _   -> (registers cpu) { r8 = x }
setRegister cpu 9 x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r9_fiq = x }
                      _   -> (registers cpu) { r9 = x }
setRegister cpu 10 x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r10_fiq = x }
                      _   -> (registers cpu) { r10 = x }
setRegister cpu 11 x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r11_fiq = x }
                      _   -> (registers cpu) { r11 = x }
setRegister cpu 12 x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r12_fiq = x }
                      _   -> (registers cpu) { r12 = x }
setRegister cpu 13 x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      User -> (registers cpu) { r13 = x }
                      FIQ -> (registers cpu) { r13_fiq = x }
                      IRQ -> (registers cpu) { r13_irq = x }
                      Supervisor -> (registers cpu) { r13_svc = x }
                      Abort -> (registers cpu) { r13_abt = x }
                      Undefined -> (registers cpu) { r13_und = x }
                      System   -> (registers cpu) { r13 = x }
setRegister cpu 14 x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      User -> (registers cpu) { r14 = x }
                      FIQ -> (registers cpu) { r14_fiq = x }
                      IRQ -> (registers cpu) { r14_irq = x }
                      Supervisor -> (registers cpu) { r14_svc = x }
                      Abort -> (registers cpu) { r14_abt = x }
                      Undefined -> (registers cpu) { r14_und = x }
                      System   -> (registers cpu) { r14 = x }
setRegister cpu 15 x = cpu { registers = (registers cpu) { pc = x } }

-- Current program status register getters and setters
getCPSR :: CPU -> StatusRegister
setCPSR :: CPU -> StatusRegister -> CPU
getSPSR :: CPU -> StatusRegister
setSPSR :: CPU -> StatusRegister -> CPU
getConditionCodeFlags :: CPU -> ConditionCodeFlags
setConditionCodeFlags :: ConditionCodeFlags -> CPU -> CPU
getProcessorMode :: CPU -> ProcessorMode
setProcessorMode :: ProcessorMode -> CPU -> CPU
getThumbStateFlag :: CPU -> Bool
setThumbStateFlag :: CPU -> Bool -> CPU
getNFlag :: CPU -> Bool
setNFlag :: CPU -> Bool -> CPU
getZFlag :: CPU -> Bool
setZFlag :: CPU -> Bool -> CPU
getCFlag :: CPU -> Bool
setCFlag :: CPU -> Bool -> CPU
getVFlag :: CPU -> Bool
setVFlag :: CPU -> Bool -> CPU

getCPSR = cpsr
setCPSR cpu cpsr' = cpu { cpsr = cpsr' }

getSPSR cpu = case (getProcessorMode cpu) of
  Abort -> spsr_abt cpu
  FIQ -> spsr_fiq cpu
  IRQ -> spsr_irq cpu
  Supervisor -> spsr_svc cpu
  Undefined -> spsr_und cpu
  _ -> undefined
setSPSR cpu spsr' = case (getProcessorMode cpu) of
  Abort -> cpu { spsr_abt = spsr' }
  FIQ -> cpu { spsr_fiq = spsr' }
  IRQ -> cpu { spsr_irq = spsr' }
  Supervisor -> cpu { spsr_svc = spsr' }
  Undefined -> cpu { spsr_und = spsr' }
  _ -> undefined

getConditionCodeFlags = conditionCodeFlags . cpsr
setConditionCodeFlags flags cpu = cpu { cpsr = cpsr' }
  where cpsr' = (cpsr cpu) { conditionCodeFlags = flags }

getProcessorMode = processorMode . cpsr
setProcessorMode mode cpu = cpu { cpsr = cpsr' }
  where cpsr' = (cpsr cpu) { processorMode = mode }

getThumbStateFlag = thumbStateFlag . cpsr
setThumbStateFlag cpu flag = cpu { cpsr = cpsr' }
  where cpsr' = (cpsr cpu) { thumbStateFlag = flag }

getNFlag = n . conditionCodeFlags . cpsr
setNFlag cpu flag = cpu { cpsr = cpsr' }
  where cpsr' = (cpsr cpu) { conditionCodeFlags = flags }
        flags = (conditionCodeFlags . cpsr $ cpu) { n = flag }

getZFlag = z . conditionCodeFlags . cpsr
setZFlag cpu flag = cpu { cpsr = cpsr' }
  where cpsr' = (cpsr cpu) { conditionCodeFlags = flags }
        flags = (conditionCodeFlags . cpsr $ cpu) { z = flag }

getCFlag = c . conditionCodeFlags . cpsr
setCFlag cpu flag = cpu { cpsr = cpsr' }
  where cpsr' = (cpsr cpu) { conditionCodeFlags = flags }
        flags = (conditionCodeFlags . cpsr $ cpu) { c = flag }

getVFlag = v . conditionCodeFlags . cpsr
setVFlag cpu flag = cpu { cpsr = cpsr' }
  where cpsr' = (cpsr cpu) { conditionCodeFlags = flags }
        flags = (conditionCodeFlags . cpsr $ cpu) { v = flag }

statusRegisterToWord32 :: StatusRegister -> Word32
word32ToStatusRegister :: Word32 -> StatusRegister

statusRegisterToWord32 sr = nb .|. zb .|. cb .|. vb .|. ib .|. fb .|. tb .|. mb
  where nb = if (n . conditionCodeFlags $ sr) then bit 31 else 0
        zb = if (z . conditionCodeFlags $ sr) then bit 30 else 0
        cb = if (c . conditionCodeFlags $ sr) then bit 29 else 0
        vb = if (c . conditionCodeFlags $ sr) then bit 28 else 0
        ib = if (irqInterruptMask sr) then bit 7 else 0
        fb = if (fiqInterruptMask sr) then bit 6 else 0
        tb = if (thumbStateFlag sr) then bit 5 else 0
        mb = case (processorMode sr) of
               User        -> 0x10
               FIQ         -> 0x11
               IRQ         -> 0x12
               Supervisor  -> 0x13
               Abort       -> 0x17
               Undefined   -> 0x1B
               System      -> 0x1F

word32ToStatusRegister w = StatusRegister
                             {
                               conditionCodeFlags = ConditionCodeFlags
                                 {
                                   n = n',
                                   z = z',
                                   c = c',
                                   v = v'
                                 },
                               irqInterruptMask = irq',
                               fiqInterruptMask = fiq',
                               thumbStateFlag = thumb',
                               processorMode = mode'
                             }
  where n' = testBit w 31
        z' = testBit w 30
        c' = testBit w 29
        v' = testBit w 28
        irq' = testBit w 7
        fiq' = testBit w 6
        thumb' = testBit w 5
        mode' = case (w .&. 0x1F) of
                  0x10 -> User
                  0x11 -> FIQ
                  0x12 -> IRQ
                  0x13 -> Supervisor
                  0x17 -> Abort
                  0x1B -> Undefined
                  0x1F -> System
