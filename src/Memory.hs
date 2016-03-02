module Memory where

import Data.Word
import Data.Bits
import qualified Data.Map as Map

-- Little endian reads and writes to the address space
type Address = Word32

data Memory = Memory {
  systemROM :: SystemROM,
  ewram :: EWRAM,
  iwram :: IWRAM,
  ioram :: IORAM,
  paletteRAM :: PaletteRAM,
  vram :: VRAM,
  oam :: OAM
} deriving (Eq, Show)

data SystemROM = SystemROM (Map.Map Address Word32) deriving (Eq, Show)
data EWRAM = EWRAM (Map.Map Address Word32) deriving (Eq, Show)
data IWRAM = IWRAM (Map.Map Address Word32) deriving (Eq, Show)
data IORAM = IORAM (Map.Map Address Word32) deriving (Eq, Show)
data PaletteRAM = PaletteRAM (Map.Map Address Word32) deriving (Eq, Show)
data VRAM = VRAM (Map.Map Address Word32) deriving (Eq, Show)
data OAM = OAM (Map.Map Address Word32) deriving (Eq, Show)

class MemoryRegion m where
  read8 :: Address -> m -> Word32
  read16 :: Address -> m -> Word32
  read32 :: Address -> m -> Word32
  write8 :: Address -> Word32 -> m -> m
  write16 :: Address -> Word32 -> m -> m
  write32 :: Address -> Word32 -> m -> m

  -- Default little endian implementations
  read16 a m = read8 a m .|. (read8 (a + 1) m `shiftL` 8)
  read32 a m = read16 a m .|. (read8 (a + 2) m `shiftL` 16)
  write16 a w = (write8 (a+1) w2) . (write8 a w1)
    where w1 = w .&. 0x00FF
          w2 = (w .&. 0xFF00) `shiftR` 8
  write32 a w = (write16 (a+2) w2) . (write16 a w1)
    where w1 = w .&. 0x0000FFFF
          w2 = (w .&. 0xFFFF0000) `shiftR` 16

-- TODO: This is special and reads need to return the current prefetched instruction. 
instance MemoryRegion SystemROM where
  read8 address (SystemROM memory) = maybe 0 id value
    where value = Map.lookup address memory
  write8 address byte = id

instance MemoryRegion EWRAM where
  read8 a (EWRAM m) = maybe 0 id b
    where b = Map.lookup (a .&. 0x3FFFF) m
  write8 a b (EWRAM m) = EWRAM $ Map.insert (a .&. 0x3FFFF) b m

instance MemoryRegion IWRAM where
  read8 a (IWRAM m) = maybe 0 id b
    where b = Map.lookup (a .&. 0x7FFF) m
  write8 a b (IWRAM m) = IWRAM $ Map.insert (a .&. 0x7FFF) b m

-- TODO: I don't quite understand how memory mirroring works here.
instance MemoryRegion IORAM where
  read8 a (IORAM m) = maybe 0 id b
    where b = Map.lookup (a .&. 0x3FF) m
  write8 a b (IORAM m) = IORAM $ Map.insert (a .&. 0x3FF) b m

instance MemoryRegion PaletteRAM where
  read8 a (PaletteRAM m) = maybe 0 id b
    where b = Map.lookup (a .&. 0x3FF) m
  write8 a b (PaletteRAM m) = PaletteRAM $ Map.insert (a .&. 0x3FF) b m

-- TODO: The memory mirroring here is incorrect.
instance MemoryRegion VRAM where
  read8 a (VRAM m) = maybe 0 id b
    where b = Map.lookup (a .&. 0x1FFFF) m
  write8 a b (VRAM m) = VRAM $ Map.insert (a .&. 0x1FFFF) b m

instance MemoryRegion OAM where
  read8 a (OAM m) = maybe 0 id b
    where b = Map.lookup (a .&. 0x3FF) m
  write8 a b (OAM m) = OAM $ Map.insert (a .&. 0x3FF) b m

instance MemoryRegion Memory where
  read8 a m = case (a .&. 0xFF000000) of
            0x00 -> read8 a (systemROM m)
            0x01 -> read8 a (systemROM m)
            0x02 -> read8 a (ewram m)
            0x03 -> read8 a (iwram m) 
            0x04 -> read8 a (ioram m)
            0x05 -> read8 a (paletteRAM m)
            0x06 -> read8 a (vram m)
            0x07 -> read8 a (oam m)
            _    -> 0
  write8 a b m = case (a .&. 0xFF000000) of
            0x00 -> m { systemROM = write8 a b (systemROM m) }
            0x01 -> m { systemROM = write8 a b (systemROM m) }
            0x02 -> m { ewram = write8 a b (ewram m) }
            0x03 -> m { iwram = write8 a b (iwram m) }
            0x04 -> m { ioram = write8 a b (ioram m) }
            0x05 -> m { paletteRAM = write8 a b (paletteRAM m) }
            0x06 -> m { vram = write8 a b (vram m) } 
            0x07 -> m { oam = write8 a b (oam m) } 
            _    -> m
