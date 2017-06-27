module Pep.Header.COFF.Machine
  ( Machine(..)
  , fromMachine
  , toMachine
  ) where

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word16)

data Machine = IMAGE_FILE_MACHINE_UNKNOWN
             | IMAGE_FILE_MACHINE_AM33
             | IMAGE_FILE_MACHINE_AMD64
             | IMAGE_FILE_MACHINE_ARM
             | IMAGE_FILE_MACHINE_ARMNT
             | IMAGE_FILE_MACHINE_ARM64
             | IMAGE_FILE_MACHINE_EBC
             | IMAGE_FILE_MACHINE_I386
             | IMAGE_FILE_MACHINE_IA64
             | IMAGE_FILE_MACHINE_M32R
             | IMAGE_FILE_MACHINE_MIPS16
             | IMAGE_FILE_MACHINE_MIPSFPU
             | IMAGE_FILE_MACHINE_MIPSFPU16
             | IMAGE_FILE_MACHINE_POWERPC
             | IMAGE_FILE_MACHINE_POWERPCFP
             | IMAGE_FILE_MACHINE_R4000
             | IMAGE_FILE_MACHINE_SH3
             | IMAGE_FILE_MACHINE_SH3DSP
             | IMAGE_FILE_MACHINE_SH4
             | IMAGE_FILE_MACHINE_SH5
             | IMAGE_FILE_MACHINE_THUMB
             | IMAGE_FILE_MACHINE_WCEMIPSV2
             deriving (Bounded, Enum, Eq, Show)

instance Serialize Machine where
  get = do
    machine <- toMachine <$> Cereal.getWord16le
    maybe (fail "Machine type in COFF header is invalid") pure machine

  put = Cereal.putWord16le . fromMachine

fromMachine :: Machine -> Word16
fromMachine IMAGE_FILE_MACHINE_UNKNOWN   = 0x0000
fromMachine IMAGE_FILE_MACHINE_AM33      = 0x01d3
fromMachine IMAGE_FILE_MACHINE_AMD64     = 0x8664
fromMachine IMAGE_FILE_MACHINE_ARM       = 0x01c0
fromMachine IMAGE_FILE_MACHINE_ARMNT     = 0x01c4
fromMachine IMAGE_FILE_MACHINE_ARM64     = 0xaa64
fromMachine IMAGE_FILE_MACHINE_EBC       = 0x0ebc
fromMachine IMAGE_FILE_MACHINE_I386      = 0x014c
fromMachine IMAGE_FILE_MACHINE_IA64      = 0x0200
fromMachine IMAGE_FILE_MACHINE_M32R      = 0x9041
fromMachine IMAGE_FILE_MACHINE_MIPS16    = 0x0266
fromMachine IMAGE_FILE_MACHINE_MIPSFPU   = 0x0366
fromMachine IMAGE_FILE_MACHINE_MIPSFPU16 = 0x0466
fromMachine IMAGE_FILE_MACHINE_POWERPC   = 0x01f0
fromMachine IMAGE_FILE_MACHINE_POWERPCFP = 0x01f1
fromMachine IMAGE_FILE_MACHINE_R4000     = 0x0166
fromMachine IMAGE_FILE_MACHINE_SH3       = 0x01a2
fromMachine IMAGE_FILE_MACHINE_SH3DSP    = 0x01a3
fromMachine IMAGE_FILE_MACHINE_SH4       = 0x01a6
fromMachine IMAGE_FILE_MACHINE_SH5       = 0x01a8
fromMachine IMAGE_FILE_MACHINE_THUMB     = 0x01c2
fromMachine IMAGE_FILE_MACHINE_WCEMIPSV2 = 0x0169

toMachine :: Word16 -> Maybe Machine
toMachine 0x0000 = Just IMAGE_FILE_MACHINE_UNKNOWN
toMachine 0x01d3 = Just IMAGE_FILE_MACHINE_AM33
toMachine 0x8664 = Just IMAGE_FILE_MACHINE_AMD64
toMachine 0x01c0 = Just IMAGE_FILE_MACHINE_ARM
toMachine 0x01c4 = Just IMAGE_FILE_MACHINE_ARMNT
toMachine 0xaa64 = Just IMAGE_FILE_MACHINE_ARM64
toMachine 0x0ebc = Just IMAGE_FILE_MACHINE_EBC
toMachine 0x014c = Just IMAGE_FILE_MACHINE_I386
toMachine 0x0200 = Just IMAGE_FILE_MACHINE_IA64
toMachine 0x9041 = Just IMAGE_FILE_MACHINE_M32R
toMachine 0x0266 = Just IMAGE_FILE_MACHINE_MIPS16
toMachine 0x0366 = Just IMAGE_FILE_MACHINE_MIPSFPU
toMachine 0x0466 = Just IMAGE_FILE_MACHINE_MIPSFPU16
toMachine 0x01f0 = Just IMAGE_FILE_MACHINE_POWERPC
toMachine 0x01f1 = Just IMAGE_FILE_MACHINE_POWERPCFP
toMachine 0x0166 = Just IMAGE_FILE_MACHINE_R4000
toMachine 0x01a2 = Just IMAGE_FILE_MACHINE_SH3
toMachine 0x01a3 = Just IMAGE_FILE_MACHINE_SH3DSP
toMachine 0x01a6 = Just IMAGE_FILE_MACHINE_SH4
toMachine 0x01a8 = Just IMAGE_FILE_MACHINE_SH5
toMachine 0x01c2 = Just IMAGE_FILE_MACHINE_THUMB
toMachine 0x0169 = Just IMAGE_FILE_MACHINE_WCEMIPSV2
toMachine _      = Nothing
