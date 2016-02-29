{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction#-}
module Getters where

import Control.Lens
import Data.Word
import Cond
import Mutable

data ProcessorMode = User
                   | FIQ
                   | IRQ
                   | Supervisor
                   | Abort
                   | Undefined
                   | System
  deriving (Eq, Show)

data CPU = CPU {
  _cpu_r0        :: Word32,
  _cpu_r1        :: Word32,
  _cpu_r2        :: Word32,
  _cpu_r3        :: Word32,
  _cpu_r4        :: Word32,
  _cpu_r5        :: Word32,
  _cpu_r6        :: Word32,
  _cpu_r7        :: Word32,
  _cpu_r8        :: Word32,
  _cpu_r8_fiq    :: Word32,
  _cpu_r9        :: Word32,
  _cpu_r9_fiq    :: Word32,
  _cpu_r10       :: Word32,
  _cpu_r10_fiq   :: Word32,
  _cpu_r11       :: Word32,
  _cpu_r11_fiq   :: Word32,
  _cpu_r12       :: Word32,
  _cpu_r12_fiq   :: Word32,
  _cpu_r13       :: Word32,
  _cpu_r13_svc   :: Word32,
  _cpu_r13_abt   :: Word32,
  _cpu_r13_und   :: Word32,
  _cpu_r13_irq   :: Word32,
  _cpu_r13_fiq   :: Word32,
  _cpu_r14       :: Word32,
  _cpu_r14_svc   :: Word32,
  _cpu_r14_abt   :: Word32,
  _cpu_r14_und   :: Word32,
  _cpu_r14_irq   :: Word32,
  _cpu_r14_fiq   :: Word32,
  _cpu_r15       :: Word32,
  _cpu_processorMode :: ProcessorMode
} deriving (Eq, Show)

makeLenses ''CPU

g :: Lens s s a a -> Get s a
s :: Lens s s a a -> Set s a
g = view
s f = \cpu x -> (over f (const $ x cpu)) $ cpu

unbankedRegister :: Lens CPU CPU Word32 Word32 -> Mutable CPU Word32
unbankedRegister r = Mutable (g r) (s r)
twoBankedRegister r r_fiq = Mutable (f g) (f s)
  where f h = _if (get processorMode .==. pure FIQ)
            $ _then (h r_fiq)
            $ _else (h r)
sixBankedRegister r r_fiq r_irq r_svc r_abt r_und = Mutable (f g) (f s)
  where f h = _if (get processorMode .==. pure FIQ)
            $ _then (h r_fiq)
            $ _elseif (get processorMode .==. pure IRQ)
            $ _then (h r_irq)
            $ _elseif (get processorMode .==. pure Supervisor)
            $ _then (h r_svc)
            $ _elseif (get processorMode .==. pure Abort)
            $ _then (h r_abt)
            $ _elseif (get processorMode .==. pure Undefined)
            $ _then (h r_und)
            $ _else (h r)

r0 = unbankedRegister cpu_r0
r1 = unbankedRegister cpu_r1
r2 = unbankedRegister cpu_r2
r3 = unbankedRegister cpu_r3
r4 = unbankedRegister cpu_r4
r5 = unbankedRegister cpu_r5
r6 = unbankedRegister cpu_r6
r7 = unbankedRegister cpu_r7
r8 = twoBankedRegister cpu_r8 cpu_r8_fiq
r9 = twoBankedRegister cpu_r9 cpu_r9_fiq
r10 = twoBankedRegister cpu_r10 cpu_r10_fiq
r11 = twoBankedRegister cpu_r11 cpu_r11_fiq
r12 = twoBankedRegister cpu_r12 cpu_r12_fiq
r13 = sixBankedRegister cpu_r13 cpu_r13_fiq cpu_r13_irq cpu_r13_svc
                        cpu_r13_abt cpu_r13_und
r14 = sixBankedRegister cpu_r14 cpu_r14_fiq cpu_r14_irq cpu_r14_svc
                        cpu_r14_abt cpu_r14_und
r15 = unbankedRegister cpu_r15

processorMode = Mutable (g cpu_processorMode) (s cpu_processorMode)
