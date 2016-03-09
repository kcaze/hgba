module Test where

import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Numeric
import System.IO

import Bits
import CPU
import Decoder
import Instruction
import Memory

instructions :: [Instruction] -> Execute
instructions [] = fromFunction id
instructions (x:xs) = fromFunction (execute (instruction x)
                                .>> execute (instructions xs))

data Pipeline = Pipeline {
  pipeline_decode :: Maybe Word32,
  pipeline_execute :: Maybe Instruction
} deriving (Eq, Show)

data GBA = GBA {
  gba_cpu :: CPU,
  gba_pipeline :: Pipeline
} deriving (Eq, Show)

powerUpGBA :: GBA
powerUpGBA = GBA {
  gba_cpu = powerUp,
  gba_pipeline = Pipeline {
    pipeline_decode = Nothing,
    pipeline_execute = Nothing
  }
}

executeGBA :: Execute -> (GBA -> GBA)
executeGBA e gba = gba { gba_cpu = cpu' }
  where cpu' = execute e (gba_cpu gba)

executeCurrent :: GBA -> GBA
executeCurrent gba = executeGBA e gba 
  where i = pipeline_execute . gba_pipeline $ gba
        e = instruction $ maybe (Instruction al NOP) id i

updatePC :: GBA -> GBA
updatePC gba = executeGBA e gba
  where e = set r15 (r15 .+ instructionSize)

executeNext :: GBA -> GBA
executeNext gba = gba { gba_pipeline = pipeline' }
  where pipeline' = (gba_pipeline gba) {
          pipeline_execute = maybe Nothing decodeInstruction d,
          pipeline_decode = Nothing
        }
        d = pipeline_decode . gba_pipeline $ gba

decodeNext :: GBA -> GBA
decodeNext gba = gba { gba_pipeline = pipeline' }
  where pipeline' = (gba_pipeline gba) { 
          pipeline_decode = Just $ get (memory32 r15) (gba_cpu gba)
        }

step :: GBA -> GBA
step = executeCurrent
   .>> executeNext
   .>> decodeNext
   .>> updatePC

pad8 s
  | length s >= 8 = s
  | otherwise = pad8 ('0':s)
prompt :: GBA -> String
prompt gba = "0x" ++ pad8 (showHex (get (r15 .- (2 .* instructionSize)) (gba_cpu gba)) "") ++ "> "

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  bios <- B.readFile "GBA.ROM"
  let gba = (decodeNext .>> executeNext .>> updatePC .>> updatePC) $
            executeGBA (loadBIOS bios) powerUpGBA
  loop gba


loop :: GBA -> IO ()
loop gba = do
  putStr $ prompt gba
  c <- hGetChar stdin
  case c of
    'q' -> do putStrLn "Goodbye."
              return ()
    ' ' -> do putStr "\n"
              loop (step gba)
    'i' -> do putStrLn $ show gba
              loop gba
    'w' -> do viewMemoryWord gba
              loop gba
    _   -> do putStrLn $ "Commands:\n" ++
                         "  'q' to quit\n" ++
                         "  spacebar to step\n" ++
                         "  'i' to view cpu\n" ++
                         "  'w' to view a word in memory"
              loop gba

viewMemoryWord :: GBA -> IO ()
viewMemoryWord gba = do
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  putStr "Enter memory address: "
  address <- read <$> hGetLine stdin
  putStrLn $ "  " ++ pad8 (showHex (get (memory32 (pure address)) (gba_cpu gba)) "")
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
