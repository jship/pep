{-# OPTIONS_GHC -Wno-orphans #-}

module Pep.PETest where

import Pep.PE

import Pep.Header.COFFTest ()
import Pep.Header.MSDOSTest ()
import Pep.Header.SignatureTest ()

import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..))

prop_Round_trip_for_PE :: Pe -> Bool
prop_Round_trip_for_PE pe =
  Cereal.decode (Cereal.encode pe) == Right pe

instance Arbitrary Pe where
  arbitrary = Pe <$> arbitrary
                 <*> arbitrary
                 <*> arbitrary
