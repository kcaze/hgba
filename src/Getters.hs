module Getters where

import Data.Word

data CPU = CPU {
  _cpu_r0 :: Word32,
  _cpu_r1 :: Word32,
  _cpu_r2 :: Word32,
  _cpu_r3 :: Word32,
  _cpu_r4 :: Word32,
  _cpu_r5 :: Word32,
  _cpu_r6 :: Word32,
  _cpu_r7 :: Word32,
  _cpu_r8        :: Word32,
  _cpu_r8_fiq    :: Word32,
  _cpu_processorMode :: Word32
} deriving (Eq, Show)

type Get = CPU -> Word32
type Set = Get -> CPU -> CPU
data Mutable = Mutable { get :: Get, set :: Set }

r0 = Mutable cpu_r0 (\x cpu -> cpu { cpu_r0 = x cpu })
r1 = Mutable cpu_r1 (\x cpu -> cpu { cpu_r1 = x cpu })
r2 = Mutable cpu_r2 (\x cpu -> cpu { cpu_r2 = x cpu })
r3 = Mutable cpu_r3 (\x cpu -> cpu { cpu_r3 = x cpu })
r4 = Mutable cpu_r4 (\x cpu -> cpu { cpu_r4 = x cpu })
r5 = Mutable cpu_r5 (\x cpu -> cpu { cpu_r5 = x cpu })
r6 = Mutable cpu_r6 (\x cpu -> cpu { cpu_r6 = x cpu })
r7 = Mutable cpu_r7 (\x cpu -> cpu { cpu_r7 = x cpu })
r8 = Mutable getr8 setr8
     where getr8 cpu = if get processorMode cpu == fiq
                       then cpu_r8_fiq cpu
                       else cpu_r8 cpu
           setr8 x cpu = if get processorMode cpu == fiq
                         then cpu { cpu_r8_fiq = x cpu }
                         else cpu { cpu_r8 = x cpu }

processorMode = Mutable cpu_processorMode (\x cpu -> cpu { cpu_processorMode = x cpu })
