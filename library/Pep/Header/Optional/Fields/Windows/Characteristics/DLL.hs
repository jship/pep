module Pep.Header.Optional.Fields.Windows.Characteristics.DLL
  ( DllCharacteristics(..)
  , DllCharacteristic(..)
  , hasDllCharacteristic
  )
  where

import Data.Bits (testBit)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word16)

newtype DllCharacteristics = DllCharacteristics
  { dllCharacteristicsBytes :: Word16
  } deriving (Eq, Show)

instance Serialize DllCharacteristics where
  get = do
    characteristics <- DllCharacteristics <$> Cereal.getWord16le
    let hasADllCharacteristic = or $
          zipWith
            ($)
            (fmap hasDllCharacteristic
              [(minBound :: DllCharacteristic) .. (maxBound :: DllCharacteristic)])
            (repeat characteristics)
    if hasADllCharacteristic
    then pure characteristics
    else fail "Invalid optional header Windows fields DLL characteristics"

  put = Cereal.putWord16le . dllCharacteristicsBytes

data DllCharacteristic = IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA
                       | IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
                       | IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY
                       | IMAGE_DLLCHARACTERISTICS_NX_COMPAT
                       | IMAGE_DLLCHARACTERISTICS_NO_ISOLATION
                       | IMAGE_DLLCHARACTERISTICS_NO_SEH
                       | IMAGE_DLLCHARACTERISTICS_NO_BIND
                       | IMAGE_DLLCHARACTERISTICS_APPCONTAINER
                       | IMAGE_DLLCHARACTERISTICS_WDM_DRIVER
                       | IMAGE_DLLCHARACTERISTICS_GUARD_CF
                       | IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE
                       deriving (Bounded, Enum, Eq, Show)

hasDllCharacteristic :: DllCharacteristic -> DllCharacteristics -> Bool
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA       (DllCharacteristics flags) = testBit flags  5
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE          (DllCharacteristics flags) = testBit flags  6
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY       (DllCharacteristics flags) = testBit flags  7
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_NX_COMPAT             (DllCharacteristics flags) = testBit flags  8
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_NO_ISOLATION          (DllCharacteristics flags) = testBit flags  9
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_NO_SEH                (DllCharacteristics flags) = testBit flags 10
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_NO_BIND               (DllCharacteristics flags) = testBit flags 11
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_APPCONTAINER          (DllCharacteristics flags) = testBit flags 12
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_WDM_DRIVER            (DllCharacteristics flags) = testBit flags 13
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_GUARD_CF              (DllCharacteristics flags) = testBit flags 14
hasDllCharacteristic IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE (DllCharacteristics flags) = testBit flags 15
