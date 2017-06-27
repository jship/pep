{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.COFF.CharacteristicsTest where

import Pep.Header.COFF.Characteristics

import qualified Control.Monad as Monad
import Data.Bits ((.|.), bit)
import Data.Foldable (foldl')
import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck
       (Arbitrary(..), Gen, Positive(..), arbitraryBoundedEnum)

prop_Round_trip_for_COFF_characteristics :: Characteristics -> Bool
prop_Round_trip_for_COFF_characteristics characteristics =
  Cereal.decode (Cereal.encode characteristics) == Right characteristics

instance Arbitrary Characteristics where
  arbitrary = do
    Positive count <- arbitrary
    flags <- Monad.replicateM count (arbitraryBoundedEnum :: Gen Characteristic)
    let intFlags = fmap (bit . fromEnum) flags
    let bytes = foldl' (.|.) 0 intFlags
    pure $ Characteristics bytes
