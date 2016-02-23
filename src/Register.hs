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
                )
                where

import Types

-- General purpose register getters and setters.
getRegister :: Int -> CPU -> Register
setRegister :: Int -> CPU -> Register -> CPU

getR0 = getRegister 0
getR1 = getRegister 1
getR2 = getRegister 2
getR3 = getRegister 3
getR4 = getRegister 4
getR5 = getRegister 5
getR6 = getRegister 6
getR7 = getRegister 7
getR8 = getRegister 8
getR9 = getRegister 9
getR10 = getRegister 10
getR11 = getRegister 11
getR12 = getRegister 12
getR13 = getRegister 13
getR14 = getRegister 14
getR15 = getRegister 15

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

getRegister 0 cpu = r0 . registers $ cpu
getRegister 1 cpu = r1 . registers $ cpu
getRegister 2 cpu = r2 . registers $ cpu
getRegister 3 cpu = r3 . registers $ cpu
getRegister 4 cpu = r4 . registers $ cpu
getRegister 5 cpu = r5 . registers $ cpu
getRegister 6 cpu = r6 . registers $ cpu
getRegister 7 cpu = r7 . registers $ cpu
getRegister 8 cpu = (case (getProcessorMode cpu) of
  FIQ -> r8_fiq . registers
  _   -> r8 . registers) $ cpu
getRegister 9 cpu = (case (getProcessorMode cpu) of
  FIQ -> r9_fiq . registers
  _   -> r9 . registers) $ cpu
getRegister 10 cpu = (case (getProcessorMode cpu) of
  FIQ -> r10_fiq . registers
  _   -> r10 . registers) $ cpu
getRegister 11 cpu = (case (getProcessorMode cpu) of
  FIQ -> r11_fiq . registers
  _   -> r11 . registers) $ cpu
getRegister 12 cpu = (case (getProcessorMode cpu) of
  FIQ -> r12_fiq . registers
  _   -> r12 . registers) $ cpu
getRegister 13 cpu = (case (getProcessorMode cpu) of
  User -> r13 . registers
  FIQ -> r13_fiq . registers
  IRQ -> r13_irq . registers
  Supervisor -> r13_svc . registers
  Abort -> r13_abt . registers
  Undefined -> r13_und . registers
  System -> r13 . registers) $ cpu
getRegister 14 cpu = (case (getProcessorMode cpu) of
  User -> r14 . registers
  FIQ -> r14_fiq . registers
  IRQ -> r14_irq . registers
  Supervisor -> r14_svc . registers
  Abort -> r14_abt . registers
  Undefined -> r14_und . registers
  System -> r14 . registers) $ cpu
getRegister 15 cpu = pc . registers $ cpu

setRegister 0 cpu x = cpu { registers = (registers cpu) { r0 = x } }
setRegister 1 cpu x = cpu { registers = (registers cpu) { r1 = x } }
setRegister 2 cpu x = cpu { registers = (registers cpu) { r2 = x } }
setRegister 3 cpu x = cpu { registers = (registers cpu) { r3 = x } }
setRegister 4 cpu x = cpu { registers = (registers cpu) { r4 = x } }
setRegister 5 cpu x = cpu { registers = (registers cpu) { r5 = x } }
setRegister 6 cpu x = cpu { registers = (registers cpu) { r6 = x } }
setRegister 7 cpu x = cpu { registers = (registers cpu) { r7 = x } }
setRegister 8 cpu x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r8_fiq = x }
                      _   -> (registers cpu) { r8 = x }
setRegister 9 cpu x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r9_fiq = x }
                      _   -> (registers cpu) { r9 = x }
setRegister 10 cpu x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r10_fiq = x }
                      _   -> (registers cpu) { r10 = x }
setRegister 11 cpu x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r11_fiq = x }
                      _   -> (registers cpu) { r11 = x }
setRegister 12 cpu x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      FIQ -> (registers cpu) { r12_fiq = x }
                      _   -> (registers cpu) { r12 = x }
setRegister 13 cpu x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      User -> (registers cpu) { r13 = x }
                      FIQ -> (registers cpu) { r13_fiq = x }
                      IRQ -> (registers cpu) { r13_irq = x }
                      Supervisor -> (registers cpu) { r13_svc = x }
                      Abort -> (registers cpu) { r13_abt = x }
                      Undefined -> (registers cpu) { r13_und = x }
                      System   -> (registers cpu) { r13 = x }
setRegister 14 cpu x = cpu { registers = registers' }
  where registers' = case (getProcessorMode cpu) of
                      User -> (registers cpu) { r14 = x }
                      FIQ -> (registers cpu) { r14_fiq = x }
                      IRQ -> (registers cpu) { r14_irq = x }
                      Supervisor -> (registers cpu) { r14_svc = x }
                      Abort -> (registers cpu) { r14_abt = x }
                      Undefined -> (registers cpu) { r14_und = x }
                      System   -> (registers cpu) { r14 = x }
setRegister 15 cpu x = cpu { registers = (registers cpu) { pc = x } }

-- Current program status register getters and setters
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
