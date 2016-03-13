{-# LANGUAGE BinaryLiterals #-}
module GPU where

import Control.Applicative
import Data.Bits
import Data.Word
import Numeric
import SDL hiding (get)
import Linear
import Linear.Affine

import CPU
import Imperative
import Memory
import Types

readAddress :: Address -> (Word32 -> a) -> CPU -> a
writeAddress :: Address -> (Word32 -> Word32) -> CPU -> CPU 
readAddress a f c = c `seq` f $ read32 a (cpu_memory c)
writeAddress a f c = c `seq` c { cpu_memory = memory }
  where memory = write32 a (readAddress a f c) (cpu_memory c)

-- Register names
rDISPCNT = 0x04000000
rDISPSTAT = 0x04000004
rVCOUNT = 0x04000006
addrOAM = 0x07000000
addrOBJTiles = 0x06010000
addrBGPalette = 0x05000000
addrOBJPalette = 0x05000200

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
updateLCD = fromFunction (\cpu -> cpu `seq`(
      setVCount (cpu_cycles cpu `div` 308)
    . setVRefresh (inRange ((cpu_cycles cpu `div` 308) `mod` 228) 0 159)
    . setHRefresh (inRange (cpu_cycles cpu `mod` 308) 0 239)
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
    sprite_tile = fromIntegral $ attr2 .&. 0x3FF,
    sprite_palette = fromIntegral $ attr2 .&. 0xF000,
    sprite_256ColorEnabled = testBit attr0 13
  }
  where attr0 = read16 address cpu
        attr1 = read16 (address + 2) cpu
        attr2 = read16 (address + 4) cpu
        attr3 = read16 (address + 6) cpu
        address = addrOAM + (fromIntegral $ 8 * n)

data Sprite = Sprite {
  sprite_x :: Int,
  sprite_y :: Int,
  sprite_size :: (Int, Int),
  sprite_tile :: Int,
  sprite_palette :: Int,
  sprite_256ColorEnabled :: Bool
}

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
          renderPixel (getOBJ256Color (fromIntegral pixel) cpu) (x + dx, y + dy) renderer

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
