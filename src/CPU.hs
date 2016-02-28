module CPU where

import qualified Data.Map.Strict as Map
import Types
import qualified Decoder as D
import qualified Instruction as I
import qualified Memory as M
import qualified Register as R

step :: CPU -> CPU
step = incrementPC . fetchNext . decodeNext . executeInstruction

executeInstruction :: CPU -> CPU
executeInstruction cpu = maybe cpu (cpu `I.executeInstruction`) instruction
  where instruction = execute . pipeline $ cpu

fetchNext :: CPU -> CPU
fetchNext cpu = cpu { pipeline = pipeline' }
  where pipeline' = (pipeline cpu) { decode = undefined } -- TODO

decodeNext :: CPU -> CPU
decodeNext cpu = cpu { pipeline = p { execute = execute' } }
  where p = pipeline cpu
        execute' = maybe (execute p) D.decodeInstruction (decode p)

incrementPC :: CPU -> CPU
incrementPC cpu = cpu `R.setR15` pc'
  where pc' = (R.getR15 cpu) + (if R.getThumbStateFlag cpu then 4 else 8)

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
        sr = StatusRegister
          {
            conditionCodeFlags = ConditionCodeFlags False False False False,
            irqInterruptMask = False,
            fiqInterruptMask = False,
            thumbStateFlag = False,
            processorMode = User
          }
        p = Pipeline Nothing Nothing
        m = Map.empty
