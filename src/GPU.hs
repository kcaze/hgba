{-# LANGUAGE BinaryLiterals #-}
module GPU where

import Control.Applicative
import Data.Bits
import Data.Word
import qualified Data.Map.Strict as Map
import Numeric
import SDL hiding (get)
import Linear
import Linear.Affine

import CPU
import Imperative
import Memory
import Types

---------------------------------------------
-- Hardware register and address constants --
---------------------------------------------
rDISPCNT = 0x04000000
rDISPSTAT = 0x04000004
rVCOUNT = 0x04000006
rIE = 0x04000200
rIF = 0x04000202
rIME = 0x04000208
rKEY = 0x4000130
addrOAM = 0x07000000
addrOBJTiles = 0x06010000
addrBGPalette = 0x05000000
addrOBJPalette = 0x05000200

-- Direct read and write access to hardware IO registers
readIO8 :: Address -> (Word32 -> a) -> CPU -> a
writeIO8 :: Address -> (Word32 -> Word32) -> CPU -> CPU 
readIO16 :: Address -> (Word32 -> a) -> CPU -> a
writeIO16 :: Address -> (Word32 -> Word32) -> CPU -> CPU 
readIO32 :: Address -> (Word32 -> a) -> CPU -> a
writeIO32 :: Address -> (Word32 -> Word32) -> CPU -> CPU 

readIO8 a f c = c `seq` f $ w
  where IORAM m = ioram . cpu_memory $ c
        w = maybe 0 id (Map.lookup (a .&. 0x3FF) m)
writeIO8 a f c = c `seq` c { cpu_memory = (cpu_memory c) { ioram = m' } }
  where IORAM m = ioram . cpu_memory $ c
        m' = IORAM $ Map.insert (a .&. 0x3FF) (readIO8 a f c) m
readIO16 a f c = c `seq` f $ (w2 .|. w1)
  where w1 = readIO8 a id c
        w2 = readIO8 (a+1) id c <! 8
writeIO16 a f c = c `seq` c { cpu_memory = (cpu_memory c) { ioram = m'' } }
  where w = readIO16 a f c
        w1 = bitRange 0 7 w
        w2 = bitRange 8 15 w
        IORAM m = ioram . cpu_memory $ c
        m' = Map.insert (a .&. 0x3FF) w1 m
        m'' = IORAM $ Map.insert ((a+1) .&. 0x3FF) w2 m'
readIO32 a f c = c `seq` f $ (w2 .|. w1)
  where w1 = readIO16 a id c
        w2 = readIO16 (a+2) id c <! 16
writeIO32 a f c = c `seq` c { cpu_memory = (cpu_memory c) { ioram = m'''' } }
  where w = readIO32 a f c
        w1 = bitRange 0 7 w
        w2 = bitRange 8 15 w
        w3 = bitRange 16 23 w
        w4 = bitRange 24 31 w
        IORAM m = ioram . cpu_memory $ c
        m' = Map.insert (a .&. 0x3FF) w1 m
        m'' = Map.insert ((a+1) .&. 0x3FF) w2 m'
        m''' = Map.insert ((a+2) .&. 0x3FF) w3 m''
        m'''' = IORAM $ Map.insert ((a+3) .&. 0x3FF) w4 m'''

data VideoMode = Mode0 | Mode1 | Mode2 | Mode3 | Mode4 | Mode5
  deriving (Eq, Show)

getVideoMode :: CPU -> VideoMode
getVideoMode = readIO16 rDISPCNT (\m ->
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
setVideoMode mode = writeIO16 rDISPCNT (\w ->
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
        g n = readIO16 rDISPCNT (`testBit` n)
        s n b = writeIO16 rDISPCNT (toggleBit n b)

getVRefresh = readIO16 rDISPSTAT (`testBit` 0)
setVRefresh b = writeIO16 rDISPSTAT (toggleBit 0 b)
vRefresh = Mutable getVRefresh setVRefresh

getHRefresh = readIO16 rDISPSTAT (`testBit` 1)
setHRefresh b = writeIO16 rDISPSTAT (toggleBit 1 b)
hRefresh = Mutable getHRefresh setHRefresh

getVCount = readIO16 rVCOUNT id
setVCount w = writeIO16 rVCOUNT (const w)
vCount = Mutable getVCount setVCount

updateHardwareRegisters :: Execute
updateHardwareRegisters = fromFunction (\cpu -> cpu `seq`(
      setVCount ((cpu_cycles cpu `div` 308) `mod` 228)
    . setVRefresh (inRange ((cpu_cycles cpu `div` 308) `mod` 228) 0 159)
    . setHRefresh (inRange (cpu_cycles cpu `mod` 308) 0 239)
    . write32 rKEY 0x3FF --TODO: Handle input
    ) cpu)

  where inRange x l h = l <= x && x <= h

getBG256Color :: Int -> CPU -> Word32
getBG256Color n cpu = read16 (addrBGPalette + fromIntegral (2*n)) cpu

getOBJ256Color :: Int -> CPU -> Word32
getOBJ256Color n cpu = read16 (addrOBJPalette + fromIntegral (2*n)) cpu

getSprite :: Int -> CPU -> Sprite
getSprite n cpu = Sprite {
    sprite_x = fromIntegral $ attr1 .&. 0x1FF,
    sprite_y = fromIntegral $ attr0 .&. 0x0FF,
    sprite_size = case (bitRange 14 15 attr0, bitRange 14 15 attr1) of
                       (0b00, 0b00) -> (1, 1)
                       (0b00, 0b01) -> (2, 2)
                       (0b00, 0b10) -> (4, 4)
                       (0b00, 0b11) -> (8, 8)
                       (0b01, 0b00) -> (2, 1)
                       (0b01, 0b01) -> (4, 1)
                       (0b01, 0b10) -> (4, 2)
                       (0b01, 0b11) -> (8, 4)
                       (0b10, 0b00) -> (1, 2)
                       (0b10, 0b01) -> (1, 4)
                       (0b10, 0b10) -> (2, 4)
                       (0b10, 0b11) -> (4, 8)
                       _ -> error "Invalid sprite size.",
    sprite_mode = case (bitRange 10 11 attr0) of
                       0b00 -> NormalSprite
                       0b01 -> SemiTransparentSprite
                       0b10 -> ObjWindowSprite
                       0b11 -> error "Invalid sprite type.",
    sprite_tile = fromIntegral $ attr2 .&. 0x3FF,
    sprite_palette = fromIntegral $ attr2 .&. 0xF000,
    sprite_256ColorEnabled = testBit attr0 13
  }
  where attr0 = read16 address cpu
        attr1 = read16 (address + 2) cpu
        attr2 = read16 (address + 4) cpu
        attr3 = read16 (address + 6) cpu
        address = addrOAM + (fromIntegral $ 8 * n)

data SpriteMode = NormalSprite
                | SemiTransparentSprite
                | ObjWindowSprite
  deriving (Eq, Show)

data Sprite = Sprite {
  sprite_x :: Int,
  sprite_y :: Int,
  sprite_size :: (Int, Int),
  sprite_tile :: Int,
  sprite_palette :: Int,
  sprite_256ColorEnabled :: Bool,
  sprite_mode :: SpriteMode
} deriving (Eq, Show)

---------------
-- Renderers --
---------------
renderPixel :: Word32 -> (Int, Int) -> Renderer -> IO()
renderPixel c (x, y) renderer = do
  let (r,g,b) = (bitRange 0 4 c <! 3, bitRange 5 9 c <! 3, bitRange 10 14 c <! 3)
  rendererDrawColor renderer $= V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
  drawPoint renderer (P $ V2 (fromIntegral x) (fromIntegral y))

render256ColorTile :: Address -> Int -> (Int, Int) -> CPU -> Renderer -> IO()
render256ColorTile baseAddress n (x, y) cpu renderer = do
  foldl (>>) (return ()) (map plot (liftA2 (,) [0..7] [0..7]))
  where address = baseAddress + (fromIntegral $ n * 0x20)
        plot (dx, dy) = do
          let pixel = read8 (address + fromIntegral dx + 8 * fromIntegral dy) cpu
          if pixel /= 0 then
            renderPixel (getOBJ256Color (fromIntegral pixel) cpu) (x + dx, y + dy) renderer
          else return ()

renderSprite :: Int -> CPU -> Renderer -> IO()
renderSprite n cpu renderer =
  foldl (>>) (return ()) (map plot (liftA2 (,) [0..(w-1)] [0..(h-1)]))
  where sprite = getSprite n cpu
        (w, h) = sprite_size sprite
        plot (tx, ty) = do
          render256ColorTile (addrOBJTiles)
                             (sprite_tile sprite + 2*tx + 32*ty)
                             (sprite_x sprite + 8*tx, sprite_y sprite + 8*ty)
                             cpu
                             renderer

renderSprites :: CPU -> Renderer -> IO()
renderSprites cpu renderer = do
  foldl (>>) (return ()) (map plot [0..10])
  where plot n = renderSprite n cpu renderer

render :: CPU -> Renderer -> IO()
render cpu renderer = do
  putStrLn "Rendering frame."
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  if get oamEnabled cpu then renderSprites cpu renderer else return ()

  present renderer

----------------
-- Interrupts --
----------------
triggerInterrupts :: Execute
triggerInterrupts = fromFunction f
  where f cpu = if testBit (readIO8 rIME id cpu) 0 &&
                   not (get iFlag cpu)
                then run triggerVBlankInterrupt cpu
                else cpu

-- TODO: Implement the other interrupts
triggerVBlankInterrupt :: Execute 
triggerVBlankInterrupt = fromFunction f
  where f cpu = if testBit (readIO8 rIE id cpu) 0 &&
                   testBit (readIO8 rDISPSTAT id cpu) 3 &&
                   cpu_cycles cpu `mod` 70224 == 49280 -- TODO: Cycle accuracy
                then (run (enterException E_IRQ) (writeIO8 rIF (toggleBit 0 True) cpu))
                     { cpu_halt = False }
                else cpu
