{-# LANGUAGE RecordWildCards #-}

module Pep.Header.MSDOS
  ( MsDosStub(..)
  ) where

import qualified Control.Monad as Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word8)

data MsDosStub = MsDosStub
  { msDosStubSignature                :: !ByteString
  , msDosStubBytesToPeSignatureOffset :: !ByteString
  , msDosStubPeSignatureOffset        :: !Word8
  , msDosStubBytesToPeSignature       :: !ByteString
  } deriving (Eq, Show)

instance Serialize MsDosStub where
  get = do
    -- Need at least 0x3C bytes
    m <- Cereal.getWord8
    Monad.when (m /= 0x4D) $
      fail $ "First char of MSDOS stub is " ++ show m ++ " instead of M (0x4D)"
    z <- Cereal.getWord8
    Monad.when (z /= 0x5A) $
      fail $ "Second char of MSDOS stub is " ++ show m ++ " instead of Z (0x5A)"
    let msDosStubSignature = ByteString.pack [m, z]
    msDosStubBytesToPeSignatureOffset <- Cereal.getByteString 0x3A
    msDosStubPeSignatureOffset <- Cereal.getWord8
    msDosStubBytesToPeSignature <- Cereal.getByteString
      (fromIntegral $ msDosStubPeSignatureOffset - 0x3C - 1)
    pure $ MsDosStub{..}

  put MsDosStub{..} = do
    Cereal.putByteString msDosStubSignature
    Cereal.putByteString msDosStubBytesToPeSignatureOffset
    Cereal.putWord8 msDosStubPeSignatureOffset
    Cereal.putByteString msDosStubBytesToPeSignature
