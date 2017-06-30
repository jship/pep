{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.COFFTest where

import Pep.Header.COFF

import Pep.Header.COFF.CharacteristicsTest ()
import Pep.Header.COFF.MachineTest ()

import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..))

prop_Round_trip_for_COFF_Header :: CoffHeader -> Bool
prop_Round_trip_for_COFF_Header coffHeader =
  Cereal.decode (Cereal.encode coffHeader) == Right coffHeader

instance Arbitrary CoffHeader where
  arbitrary = CoffHeader <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
