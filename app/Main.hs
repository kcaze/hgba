{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Numeric
import System.IO
import SDL hiding (get)
import Linear
import Linear.Affine

import CPU
import Execute
import Imperative
import Types

pad8 s
  | length s >= 8 = s
  | otherwise = pad8 ('0':s)
prompt :: CPU -> String
prompt cpu = "0x" ++ pad8 (showHex (cpu_r15 cpu) "") ++ "> "

main = do
  SDL.initializeAll
  window <- createWindow "hgba" defaultWindow { windowInitialSize = V2 240 160 }
  renderer <- createRenderer window (-1) defaultRenderer

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  bios <- B.readFile "bios.bin"
  game <- B.readFile "game.bin"
  let cpu = run (loadBIOS bios .>> loadGame game) powerUp
  loop cpu renderer

loop :: CPU -> Renderer -> IO ()
loop cpu renderer = do
  {--
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255
  drawPoint renderer (P $ V2 24 24)
  present renderer
  --}

  putStr $ prompt cpu
  c <- hGetChar stdin
  case c of
    'q' -> do putStrLn "Goodbye."
              return ()
    'b' -> do cpu' <- setBreakpoint cpu
              loop cpu' renderer
    ' ' -> do putStr "\n"
              loop (run step cpu) renderer
    'i' -> do putStrLn $ show cpu
              loop cpu renderer
    'w' -> do viewMemoryWord cpu
              loop cpu renderer
    _   -> do putStrLn $ "Commands:\n" ++
                         "  'q' to quit\n" ++
                         "  spacebar to step\n" ++
                         "  'i' to view cpu\n" ++
                         "  'b' to break at an address\n" ++
                         "  'w' to view a word in memory"
              loop cpu renderer

setBreakpoint :: CPU -> IO CPU
setBreakpoint cpu = do
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  putStr "Enter breakpoint address: "
  address <- read <$> hGetLine stdin
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

  return $ breakpoint cpu address

breakpoint :: CPU -> Word32 -> CPU
breakpoint c a
  | get r15 c == a + (2 * get instructionSize c) = c
  | otherwise = breakpoint (run step c) a

viewMemoryWord :: CPU -> IO ()
viewMemoryWord cpu = do
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  putStr "Enter memory address: "
  address <- read <$> hGetLine stdin
  putStrLn $ "  " ++ pad8 (showHex (get (memory32 (pure address)) cpu) "")
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
