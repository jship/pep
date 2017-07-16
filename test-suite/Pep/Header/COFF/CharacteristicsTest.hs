{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.Header.COFF.CharacteristicsTest where

import Pep.Header.COFF.Characteristics

import qualified Control.Monad as Monad
import Data.Bits ((.|.), bit)
import Data.Foldable (foldl')
import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck
       (Arbitrary(..), Positive(..), elements)

prop_Round_trip_for_COFF_characteristics :: Characteristics -> Bool
prop_Round_trip_for_COFF_characteristics characteristics =
  Cereal.decode (Cereal.encode characteristics) == Right characteristics

instance Arbitrary Characteristics where
  arbitrary = do
    Positive count <- arbitrary
    intFlags <- fmap bit <$> Monad.replicateM count
                  (elements [0, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15])
    let bytes = foldl' (.|.) 0 intFlags
    pure $ Characteristics bytes
