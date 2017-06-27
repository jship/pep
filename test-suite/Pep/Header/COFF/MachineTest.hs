{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.COFF.MachineTest where

import Pep.Header.COFF.Machine

import qualified Data.Serialize as Cereal

import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

prop_Round_trip_for_COFF_machine_type :: Machine -> Bool
prop_Round_trip_for_COFF_machine_type machine =
  Cereal.decode (Cereal.encode machine) == Right machine

instance Arbitrary Machine where
  arbitrary = arbitraryBoundedEnum
