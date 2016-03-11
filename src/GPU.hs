{-# LANGUAGE BinaryLiterals #-}
module GPU where

import Data.Bits
import Data.Word

import CPU
import Imperative
import Memory
import Types

readAddress :: Address -> (Word32 -> a) -> CPU -> a
writeAddress :: Address -> (Word32 -> Word32) -> CPU -> CPU 
readAddress a f c = f $ read32 a (cpu_memory c)
writeAddress a f c = c { cpu_memory = memory }
  where memory = write32 a (readAddress a f c) (cpu_memory c)

-- Register names
rDISPCNT = 0x04000000
rDISPSTAT = 0x04000004
rVCOUNT = 0x04000006

data VideoMode = Mode0 | Mode1 | Mode2 | Mode3 | Mode4 | Mode5
  deriving (Eq, Show)

getVideoMode :: CPU -> VideoMode
getVideoMode = readAddress rDISPCNT (\m ->
  case m .&. 0b111 of
    0 -> Mode0
    1 -> Mode1
    2 -> Mode2
    3 -> Mode3
    4 -> Mode4
    5 -> Mode5
    _ -> error $ "GBA in invalid mode " ++ show (m .&. 0b111)
  )

setVideoMode :: VideoMode -> CPU -> CPU
setVideoMode mode = writeAddress rDISPCNT (\w ->
    (w .&. complement 0b111) .|. m
  ) where m = case mode of
                Mode0 -> 0
                Mode1 -> 1
                Mode2 -> 2
                Mode3 -> 3
                Mode4 -> 4
                Mode5 -> 5

videoMode :: Value CPU VideoMode
videoMode = Mutable getVideoMode setVideoMode

[bg0Enabled, bg1Enabled, bg2Enabled, bg3Enabled, oamEnabled,
 window0Enabled, window1Enabled, spriteWindowsEnabled] = map f [8..15]
  where f n = Mutable (g n) (s n)
        g n = readAddress rDISPCNT (`testBit` n)
        s n b = writeAddress rDISPCNT (\w -> toggleBit w n b)

getVRefresh = readAddress rDISPSTAT (`testBit` 0)
setVRefresh b = writeAddress rDISPSTAT (\w -> toggleBit w 0 b)
vRefresh = Mutable getVRefresh setVRefresh

getHRefresh = readAddress rDISPSTAT (`testBit` 1)
setHRefresh b = writeAddress rDISPSTAT (\w -> toggleBit w 1 b)
hRefresh = Mutable getHRefresh setHRefresh

getVCount = readAddress rVCOUNT id
setVCount w = writeAddress rVCOUNT (const w)
vCount = Mutable getVCount setVCount

updateLCD :: Execute
updateLCD = fromFunction (\cpu -> (
      setVCount (cpu_cycles cpu `div` 308)
  .>> setVRefresh (inRange (getVCount cpu `mod` 228) 0 159)
  .>> setHRefresh (inRange ((cpu_cycles cpu `div` 4) `mod` 308) 0 239)
  ) !cpu)
  where inRange x l h = l <= x && x <= h
