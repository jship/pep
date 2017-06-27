module Pep.Header.COFF.Characteristics
  ( Characteristics(..)
  , Characteristic(..)
  , hasCharacteristic
  )
  where

import Data.Bits (testBit)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word16)

newtype Characteristics = Characteristics
  { characteristicsBytes :: Word16
  } deriving (Eq, Show)

instance Serialize Characteristics where
  get = do
    characteristics <- Characteristics <$> Cereal.getWord16le
    let hasACharacteristic = or $
          zipWith
            ($)
            (fmap hasCharacteristic
              [(minBound :: Characteristic) .. (maxBound :: Characteristic)])
            (repeat characteristics)
    if hasACharacteristic
    then pure characteristics
    else fail "Invalid COFF header characteristics"

  put = Cereal.putWord16le . characteristicsBytes

data Characteristic = IMAGE_FILE_RELOCS_STRIPPED
                    | IMAGE_FILE_EXECUTABLE_IMAGE
                    | IMAGE_FILE_LINE_NUMS_STRIPPED
                    | IMAGE_FILE_LOCAL_SYMS_STRIPPED
                    | IMAGE_FILE_AGGRESSIVE_WS_TRIM
                    | IMAGE_FILE_LARGE_ADDRESS_AWARE
                    | IMAGE_FILE_BYTES_REVERSED_LO
                    | IMAGE_FILE_32BIT_MACHINE
                    | IMAGE_FILE_DEBUG_STRIPPED
                    | IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP
                    | IMAGE_FILE_NET_RUN_FROM_SWAP
                    | IMAGE_FILE_SYSTEM
                    | IMAGE_FILE_DLL
                    | IMAGE_FILE_UP_SYSTEM_ONLY
                    | IMAGE_FILE_BYTES_REVERSED_HI
                    deriving (Bounded, Enum, Eq, Show)

hasCharacteristic :: Characteristic -> Characteristics -> Bool
hasCharacteristic IMAGE_FILE_RELOCS_STRIPPED         (Characteristics flags) = testBit flags  0
hasCharacteristic IMAGE_FILE_EXECUTABLE_IMAGE        (Characteristics flags) = testBit flags  1
hasCharacteristic IMAGE_FILE_LINE_NUMS_STRIPPED      (Characteristics flags) = testBit flags  2
hasCharacteristic IMAGE_FILE_LOCAL_SYMS_STRIPPED     (Characteristics flags) = testBit flags  3
hasCharacteristic IMAGE_FILE_AGGRESSIVE_WS_TRIM      (Characteristics flags) = testBit flags  4
hasCharacteristic IMAGE_FILE_LARGE_ADDRESS_AWARE     (Characteristics flags) = testBit flags  5
hasCharacteristic IMAGE_FILE_BYTES_REVERSED_LO       (Characteristics flags) = testBit flags  6
hasCharacteristic IMAGE_FILE_32BIT_MACHINE           (Characteristics flags) = testBit flags  7
hasCharacteristic IMAGE_FILE_DEBUG_STRIPPED          (Characteristics flags) = testBit flags  8
hasCharacteristic IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP (Characteristics flags) = testBit flags  9
hasCharacteristic IMAGE_FILE_NET_RUN_FROM_SWAP       (Characteristics flags) = testBit flags 10
hasCharacteristic IMAGE_FILE_SYSTEM                  (Characteristics flags) = testBit flags 11
hasCharacteristic IMAGE_FILE_DLL                     (Characteristics flags) = testBit flags 12
hasCharacteristic IMAGE_FILE_UP_SYSTEM_ONLY          (Characteristics flags) = testBit flags 13
hasCharacteristic IMAGE_FILE_BYTES_REVERSED_HI       (Characteristics flags) = testBit flags 14
