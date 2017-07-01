{-# LANGUAGE RecordWildCards #-}

module Pep.PE
  ( Pe(..)
  ) where

import Pep.Header.COFF
import Pep.Header.MSDOS
import Pep.Header.Signature

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal

data Pe = Pe
  { psMsDosStub  :: !MsDosStub
  , peSignature  :: !PeSignature
  , peCoffHeader :: !CoffHeader
  } deriving (Eq, Show)

instance Serialize Pe where
  get = do
    psMsDosStub <- Cereal.get
    peSignature <- Cereal.get
    peCoffHeader <- Cereal.get
    pure $ Pe{..}

  put Pe{..} = do
    Cereal.put psMsDosStub
    Cereal.put peSignature
    Cereal.put peCoffHeader
