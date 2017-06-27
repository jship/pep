{-# LANGUAGE RecordWildCards #-}

module Pep.Header.Signature
  ( PeSignature(..)
  ) where

import qualified Control.Monad as Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal

newtype PeSignature = PeSignature
  { peSignatureBytes :: ByteString
  } deriving (Eq, Show)

instance Serialize PeSignature where
  get = do
    p <- Cereal.getWord8
    Monad.when (p /= 0x50) $
      fail $ "First char of PE signature is " ++ show p ++ " instead of P"
    e <- Cereal.getWord8
    Monad.when (e /= 0x45) $
      fail $ "Second char of PE signature is " ++ show e ++ " instead of E"
    null1 <- Cereal.getWord8
    Monad.when (null1 /= 0) $
      fail $ "Third char of PE signature is " ++ show null1 ++ " instead of 0x00"
    null2 <- Cereal.getWord8
    Monad.when (null2 /= 0) $
      fail $ "Fourth char of PE signature is " ++ show null2 ++ " instead of 0x00"
    let peSignatureBytes = ByteString.pack [p, e, null1, null2]
    pure $ PeSignature{..}

  put PeSignature{..} = Cereal.putByteString peSignatureBytes
