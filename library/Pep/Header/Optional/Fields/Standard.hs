{-# LANGUAGE RecordWildCards #-}

module Pep.Header.Optional.Fields.Standard
  ( StandardFields(..)
  ) where

import Pep.Header.Optional.Fields.Standard.Magic

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word8, Word32)

data StandardFields = StandardFields
  { standardFieldsMagic                   :: !Magic
  , standardFieldsMajorLinkerVersion      :: !Word8
  , standardFieldsMinorLinkerVersion      :: !Word8
  , standardFieldsSizeOfCode              :: !Word32
  , standardFieldsSizeOfInitializedData   :: !Word32
  , standardFieldsSizeOfUninitializedData :: !Word32
  , standardFieldsAddressOfEntryPoint     :: !Word32
  , standardFieldsBaseOfCode              :: !Word32
  , standardFieldsBaseOfData              :: Maybe Word32 -- only exists if PE32 and not PE32+
  } deriving (Eq, Show)

instance Serialize StandardFields where
  get = do
    standardFieldsMagic <- Cereal.get
    standardFieldsMajorLinkerVersion <- Cereal.getWord8
    standardFieldsMinorLinkerVersion <- Cereal.getWord8
    standardFieldsSizeOfCode <- Cereal.getWord32le
    standardFieldsSizeOfInitializedData <- Cereal.getWord32le
    standardFieldsSizeOfUninitializedData <- Cereal.getWord32le
    standardFieldsAddressOfEntryPoint <- Cereal.getWord32le
    standardFieldsBaseOfCode <- Cereal.getWord32le
    if standardFieldsMagic == PE32
      then do
        standardFieldsBaseOfData <- Just <$> Cereal.getWord32le
        pure $ StandardFields{..}
      else do
        let standardFieldsBaseOfData = Nothing
        pure $ StandardFields{..}

  put StandardFields{..} = do
    Cereal.put standardFieldsMagic
    Cereal.putWord8 standardFieldsMajorLinkerVersion
    Cereal.putWord8 standardFieldsMinorLinkerVersion
    Cereal.putWord32le standardFieldsSizeOfCode
    Cereal.putWord32le standardFieldsSizeOfInitializedData
    Cereal.putWord32le standardFieldsSizeOfUninitializedData
    Cereal.putWord32le standardFieldsAddressOfEntryPoint
    Cereal.putWord32le standardFieldsBaseOfCode
    maybe (pure ()) Cereal.putWord32le standardFieldsBaseOfData
