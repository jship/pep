module Pep.Header.Optional.Fields.Windows
  ( WindowsFields(..)
  ) where

import Pep.Header.Optional.Fields.Windows.Characteristics.DLL
import Pep.Header.Optional.Fields.Windows.Subsystem

import Data.Word (Word16, Word32, Word64)

data WindowsFields = WindowsFields
  { windowsFieldsImageBase                   :: !Word64 -- 4 bytes if PE32, 8 bytes if PE32+
  , windowsFieldsSectionAlignment            :: !Word32
  , windowsFieldsFileAlignment               :: !Word32
  , windowsFieldsMajorOperatingSystemVersion :: !Word16
  , windowsFieldsMinorOperatingSystemVersion :: !Word16
  , windowsFieldsMajorImageVersion           :: !Word16
  , windowsFieldsMinorImageVersion           :: !Word16
  , windowsFieldsMajorSubsystemVersion       :: !Word16
  , windowsFieldsMinorSubsystemVersion       :: !Word16
  , windowsFieldsWin32VersionValue           :: !Word32
  , windowsFieldsSizeOfImage                 :: !Word32
  , windowsFieldsSizeOfHeaders               :: !Word32
  , windowsFieldsCheckSum                    :: !Word32
  , windowsFieldsSubsystem                   :: !Subsystem
  , windowsFieldsDllCharacteristics          :: !DllCharacteristics
  , windowsFieldsSizeOfStackReserve          :: !Word64 -- 4 bytes if PE32, 8 bytes if PE32+
  , windowsFieldsSizeOfStackCommit           :: !Word64 -- 4 bytes if PE32, 8 bytes if PE32+
  , windowsFieldsSizeOfHeapReserve           :: !Word64 -- 4 bytes if PE32, 8 bytes if PE32+
  , windowsFieldsSizeOfHeapCommit            :: !Word64 -- 4 bytes if PE32, 8 bytes if PE32+
  , windowsFieldsLoaderFlags                 :: !Word32
  , windowsFieldsNumberOfRvaAndSizes         :: !Word32
  } deriving (Eq, Show)

-- Note: No Serialize instance is provided here as parsing WindowsFields
-- depends on the value of standardFieldsMagic from StandardFields.
-- Instead, an aggregate Serialize instance is provided in
-- Pep.Header.Optional.Fields that handles both StandardFields and
-- WindowsFields in one shot.
