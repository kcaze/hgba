module GBA where

import Data.Word
import qualified Data.ByteString.Lazy as B

data GBA = GBA { cpu :: CPU, mem :: Memory }

newGBA :: GBA
newGBA = GBA { cpu = c, mem = m } where
  c = CPU {
    r0 = 0, r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0, r6 = 0, r7 = 0,
    r8 = 0, r9 = 0, r10 = 0, r11 = 0, r12 = 0, sp = 0, lr = 0, pc = 0,
    zflag = False, nflag = False, cflag = False, vflag = False,
    mode = ARM
  }
  m = B.repeat 0

loadROM :: GBA -> B.ByteString -> GBA
loadROM gba rom = GBA { cpu = c', mem = m' } where
  c' = c { pc = 0x08000000 }
  c = cpu gba
  m' = B.append m rom
  m = B.take 0x08000000 (mem gba)
