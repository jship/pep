module Pep.Header.SignatureTest where

import Pep.Header.Signature

import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal

import Test.Tasty.HUnit ((@?=))

case_Round_trip_for_PE_signature :: IO ()
case_Round_trip_for_PE_signature =
  Cereal.decode (Cereal.encode peSignature) @?= Right peSignature

peSignature :: PeSignature
peSignature = PeSignature $ ByteString.pack [0x50, 0x45, 0x00, 0x00]
