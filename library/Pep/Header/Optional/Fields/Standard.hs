module Pep.Header.Optional.Fields.Standard
  ( StandardFields(..)
  ) where

import Pep.Header.Optional.Fields.Standard.Magic

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

-- Note: No Serialize instance is provided here as parsing WindowsFields
-- depends on the value of standardFieldsMagic. Instead, an aggregate
-- Serialize instance is provided in Pep.Header.Optional.Fields that
-- handles both StandardFields and WindowsFields in one shot.
