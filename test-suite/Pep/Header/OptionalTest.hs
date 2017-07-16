{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.OptionalTest where

import Pep.Header.Optional

import Pep.Header.Optional.FieldsTest ()

import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..))

prop_Round_trip_for_optional_Header :: OptionalHeader -> Bool
prop_Round_trip_for_optional_Header optionalHeader =
  Cereal.decode (Cereal.encode optionalHeader) == Right optionalHeader

instance Arbitrary OptionalHeader where
  arbitrary = OptionalHeader <$> arbitrary
