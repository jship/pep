{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Pep.Header.MSDOSTest where

import Pep.Header.MSDOS

import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..), vector)

prop_Round_trip_for_MSDOS_Stub :: MsDosStub -> Bool
prop_Round_trip_for_MSDOS_Stub msDosStub =
  Cereal.decode (Cereal.encode msDosStub) == Right msDosStub

instance Arbitrary MsDosStub where
  arbitrary = do
    let msDosStubSignature = ByteString.pack [0x4D, 0x5A]
    msDosStubBytesToPeSignatureOffset <- ByteString.pack <$> vector 0x3A
    msDosStubPeSignatureOffset <- arbitrary
    msDosStubBytesToPeSignature <- ByteString.pack <$> vector
      (fromIntegral $ msDosStubPeSignatureOffset - 0x3C - 1)
    pure $ MsDosStub {..}
