{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Pep.Header.Optional.Fields.StandardTest where

import Pep.Header.Optional.Fields.Standard
import Pep.Header.Optional.Fields.Standard.Magic

import Pep.Header.Optional.Fields.Standard.MagicTest ()

import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..))

prop_Round_trip_for_optional_header_standard_fields :: StandardFields -> Bool
prop_Round_trip_for_optional_header_standard_fields standardFields =
  Cereal.decode (Cereal.encode standardFields) == Right standardFields

instance Arbitrary StandardFields where
  arbitrary = do
    standardFieldsMagic <- arbitrary
    standardFieldsMajorLinkerVersion <- arbitrary
    standardFieldsMinorLinkerVersion <- arbitrary
    standardFieldsSizeOfCode <- arbitrary
    standardFieldsSizeOfInitializedData <- arbitrary
    standardFieldsSizeOfUninitializedData <- arbitrary
    standardFieldsAddressOfEntryPoint <- arbitrary
    standardFieldsBaseOfCode <- arbitrary
    if standardFieldsMagic == PE32
      then do
        standardFieldsBaseOfData <- Just <$> arbitrary
        pure $ StandardFields{..}
      else do
        let standardFieldsBaseOfData = Nothing
        pure $ StandardFields{..}
