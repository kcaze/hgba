module CPU (
  Memory,
  CPU,
  newCPU) where

import Data.Word
import qualified Data.ByteString.Lazy as B

type Memory = B.ByteString
type Register = Word32
data Instruction = ARMInstruction Word32 | THUMBInstruction Word32
data Mode = THUMB | ARM deriving Show
data CPSR = CPSR {
  cpsr_z :: Bool,
  cpsr_n :: Bool,
  cpsr_c :: Bool,
  cpsr_v :: Bool,
  cpsr_mode :: Mode
} deriving Show
data CPU = CPU {
  cpu_r0 :: Register, cpu_r1 :: Register, cpu_r2 :: Register,
  cpu_r3 :: Register, cpu_r4 :: Register, cpu_r5 :: Register,
  cpu_r6 :: Register, cpu_r7 :: Register, cpu_r8 :: Register,
  cpu_r9 :: Register, cpu_r10 :: Register, cpu_r11 :: Register,
  cpu_r12 :: Register, cpu_sp :: Register, cpu_lr :: Register,
  cpu_pc :: Register,
  cpu_cpsr :: CPSR,
  cpu_mem :: Memory
} deriving Show
type Cond = CPSR -> Bool

-- Create initial CPU and CPSR values
newCPU :: CPU
newCPSR :: CPSR
newMemory :: Memory

-- Set and get register values
set :: CPU -> Int -> Register -> CPU
get :: CPU -> Int -> Register

-- ARM conditions
eq :: Cond
ne :: Cond
cs :: Cond
cc :: Cond
hs :: Cond
lo :: Cond
mi :: Cond
pl :: Cond
vs :: Cond
vc :: Cond
hi :: Cond
ls :: Cond
ge :: Cond
lt :: Cond
le :: Cond
al :: Cond
nv :: Cond

-- Definitions
newCPU = CPU 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0
             newCPSR
             newMemory
newCPSR = CPSR False False False False ARM
newMemory = B.repeat 0

set cpu 0 x = cpu { cpu_r0 = x }
set cpu 1 x = cpu { cpu_r1 = x }
set cpu 2 x = cpu { cpu_r2 = x }
set cpu 3 x = cpu { cpu_r3 = x }
set cpu 4 x = cpu { cpu_r4 = x }
set cpu 5 x = cpu { cpu_r5 = x }
set cpu 6 x = cpu { cpu_r6 = x }
set cpu 7 x = cpu { cpu_r7 = x }
set cpu 8 x = cpu { cpu_r8 = x }
set cpu 9 x = cpu { cpu_r9 = x }
set cpu 10 x = cpu { cpu_r10 = x }
set cpu 11 x = cpu { cpu_r11 = x }
set cpu 12 x = cpu { cpu_r12 = x }
set cpu 13 x = cpu { cpu_sp = x }
set cpu 14 x = cpu { cpu_lr = x }
set cpu 15 x = cpu { cpu_pc = x }

get cpu n = (cpu_r!!n) cpu where
  cpu_r = [cpu_r0, cpu_r1, cpu_r2, cpu_r3, cpu_r4, cpu_r5, cpu_r6, cpu_r7,
           cpu_r8, cpu_r9, cpu_r10, cpu_r11, cpu_r12, cpu_sp, cpu_lr, cpu_pc]

eq c = cpsr_z c
ne c = not . cpsr_z $ c
cs c = cpsr_c c
cc c = not . cpsr_c $ c
hs = cs
lo = cc
mi c = cpsr_n c
pl c = not . cpsr_n $ c
vs c = cpsr_v c
vc c = not . cpsr_v $ c
hi c = (cpsr_c c) && (not . cpsr_z $ c)
ls c = (not . cpsr_c $ c) && (cpsr_z c)
ge c = cpsr_n c == cpsr_v c
lt = not . ge
gt c = (not . cpsr_z $ c) && (cpsr_n c == cpsr_v c)
le c = (cpsr_z c) && (cpsr_n c /= cpsr_v c)
al c = True
nv c = False
