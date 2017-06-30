{-# LANGUAGE RecordWildCards #-}

module Pep.Header.COFF
  ( CoffHeader(..)
  ) where

import Pep.Header.COFF.Characteristics
import Pep.Header.COFF.Machine

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word16, Word32)

data CoffHeader = CoffHeader
  { coffHeaderMachine              :: !Machine
  , coffHeaderNumberOfSections     :: !Word16
  , coffHeaderTimeDateStamp        :: !Word32
  , coffHeaderPointerToSymbolTable :: !Word32
  , coffHeaderNumberOfSymbols      :: !Word32
  , coffHeaderSizeOfOptionalHeader :: !Word16
  , coffHeaderCharacteristics      :: !Characteristics
  } deriving (Eq, Show)

instance Serialize CoffHeader where
  get = do
    coffHeaderMachine <- Cereal.get
    coffHeaderNumberOfSections <- Cereal.getWord16le
    coffHeaderTimeDateStamp <- Cereal.getWord32le
    coffHeaderPointerToSymbolTable <- Cereal.getWord32le
    coffHeaderNumberOfSymbols <- Cereal.getWord32le
    coffHeaderSizeOfOptionalHeader <- Cereal.getWord16le
    coffHeaderCharacteristics <- Cereal.get
    pure $ CoffHeader{..}

  put CoffHeader{..} = do
    Cereal.put coffHeaderMachine
    Cereal.putWord16le coffHeaderNumberOfSections
    Cereal.putWord32le coffHeaderTimeDateStamp
    Cereal.putWord32le coffHeaderPointerToSymbolTable
    Cereal.putWord32le coffHeaderNumberOfSymbols
    Cereal.putWord16le coffHeaderSizeOfOptionalHeader
    Cereal.put coffHeaderCharacteristics
