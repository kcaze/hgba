module Memory where

import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Types

-- Little endian reads and writes to the address space

read8 :: Address -> AddressSpace -> Word8
read16 :: Address -> AddressSpace -> Word16
read32 :: Address -> AddressSpace -> Word32
write8 :: Address -> Word8 -> AddressSpace -> AddressSpace
write16 :: Address -> Word16 -> AddressSpace -> AddressSpace
{--write16 :: AddressSpace -> Address -> Word16 -> AddressSpace
write32 :: AddressSpace -> Address -> Word32 -> AddressSpace--}

read8 address memory = fromIntegral $ maybe 0 id (Map.lookup address memory)
read16 address memory = byte1 .|. byte0
  where byte0 = fromIntegral $ read8 address memory
        byte1 = (`shiftL` 8) . fromIntegral $ read8 (address + 1) memory
read32 address memory = halfword1 .|. halfword0
  where halfword0 = fromIntegral $ read16 address memory
        halfword1 = (`shiftL` 16) . fromIntegral $ read16 (address + 2) memory

write8 = Map.insert
write16 address value = (write8 address1 byte1) . (write8 address0 byte0)
  where address0 = address
        address1 = address + 1
        byte0 = fromIntegral $ value .&. 0x00FF
        byte1 = fromIntegral $ (value .&. 0xFF00) `shiftR` 8
write32 address value = (write16 address1 halfword1) . (write16 address0 halfword0)
  where address0 = address
        address1 = address + 2
        halfword0 = fromIntegral $ value .&. 0x0000FFFF
        halfword1 = fromIntegral $ (value .&. 0x0000FFFF) `shiftR` 16
