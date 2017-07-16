{-# LANGUAGE RecordWildCards #-}

module Pep.Header.Optional
  ( OptionalHeader(..)
  ) where

import Pep.Header.Optional.Fields

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal

data OptionalHeader = OptionalHeader
  { optionalHeaderFields :: !Fields
  } deriving (Eq, Show)

instance Serialize OptionalHeader where
  get = do
    optionalHeaderFields <- Cereal.get
    pure $ OptionalHeader{..}

  put OptionalHeader{..} = do
    Cereal.put optionalHeaderFields
