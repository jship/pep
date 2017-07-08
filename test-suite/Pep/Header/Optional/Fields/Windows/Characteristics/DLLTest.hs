{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.Optional.Fields.Windows.Characteristics.DLLTest where

import Pep.Header.Optional.Fields.Windows.Characteristics.DLL

import qualified Control.Monad as Monad
import Data.Bits ((.|.), bit)
import Data.Foldable (foldl')
import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck
       (Arbitrary(..), Gen, Positive(..), arbitraryBoundedEnum)

prop_Round_trip_for_optional_header_windows_fields_DLL_characteristics :: DllCharacteristics -> Bool
prop_Round_trip_for_optional_header_windows_fields_DLL_characteristics dllCharacteristics =
  Cereal.decode (Cereal.encode dllCharacteristics) == Right dllCharacteristics

instance Arbitrary DllCharacteristics where
  arbitrary = do
    Positive count <- arbitrary
    flags <- Monad.replicateM count (arbitraryBoundedEnum :: Gen DllCharacteristic)
    let intFlags = fmap (bit . (+ 5) . fromEnum) flags
    let bytes = foldl' (.|.) 0 intFlags
    pure $ DllCharacteristics bytes
