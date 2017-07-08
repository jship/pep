{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.Optional.Fields.Standard.MagicTest where

import Pep.Header.Optional.Fields.Standard.Magic

import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

prop_Round_trip_for_optional_header_magic_number :: Magic -> Bool
prop_Round_trip_for_optional_header_magic_number magic =
  Cereal.decode (Cereal.encode magic) == Right magic

instance Arbitrary Magic where
  arbitrary = arbitraryBoundedEnum
