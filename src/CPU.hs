module CPU where

import qualified Data.Map.Strict as Map
import Types
import Processor

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
    pipeline = p,
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
        p = Pipeline Nothing Nothing
        m = Map.empty
