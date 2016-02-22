module CPU where

import qualified Data.Map.Strict as Map
import Types
import Processor

data CPU = CPU
  {
    registers :: GeneralPurposeRegisters,
    cpsr :: StatusRegisters,
    spsr_abt :: StatusRegisters,
    spsr_fiq :: StatusRegisters,
    spsr_irq :: StatusRegisters,
    spsr_svc :: StatusRegisters,
    spsr_und :: StatusRegisters,
    memory :: AddressSpace
  } deriving Show

reset :: CPU -> CPU
reset cpu = cpu { 
              registers = (registers cpu)
                {
                  r14_svc = pc (registers cpu),
                  pc = 0
                },
              spsr_svc = cpsr cpu,
              cpsr = (cpsr cpu)
                {
                  irqInterruptMask = True,
                  fiqInterruptMask = True,
                  processorMode = Supervisor
                }
            }

powerUp :: CPU
powerUp = reset $ CPU
  {
    registers = r,
    cpsr = sr,
    spsr_abt = sr,
    spsr_fiq = sr,
    spsr_irq = sr,
    spsr_svc = sr,
    spsr_und = sr,
    memory = m
  }
  where r = GeneralPurposeRegisters 0 0 0 0 0 0 0 0
                                    0 0 0 0 0 0 0 0
                                    0 0 0 0 0 0 0 0
                                    0 0 0 0 0 0 0
        sr = StatusRegisters
          {
            conditionCodeFlags = ConditionCodeFlags False False False False,
            irqInterruptMask = False,
            fiqInterruptMask = False,
            thumbStateFlag = False,
            processorMode = User
          }
        m = Map.empty
