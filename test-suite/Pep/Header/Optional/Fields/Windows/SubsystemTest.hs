{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.Optional.Fields.Windows.SubsystemTest where

import Pep.Header.Optional.Fields.Windows.Subsystem

import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

prop_Round_trip_for_optional_header_windows_fields_subsystem :: Subsystem -> Bool
prop_Round_trip_for_optional_header_windows_fields_subsystem subsystem =
  Cereal.decode (Cereal.encode subsystem) == Right subsystem

instance Arbitrary Subsystem where
  arbitrary = arbitraryBoundedEnum
