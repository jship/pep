{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Pep.Header.Optional.FieldsTest where

import Pep.Header.Optional.Fields
import Pep.Header.Optional.Fields.Standard
import Pep.Header.Optional.Fields.Standard.Magic
import Pep.Header.Optional.Fields.Windows

import Data.Word (Word32, Word64)

import Pep.Header.Optional.Fields.Standard.MagicTest ()
import Pep.Header.Optional.Fields.Windows.Characteristics.DLLTest ()
import Pep.Header.Optional.Fields.Windows.SubsystemTest ()

import qualified Data.Serialize as Cereal
import Test.Tasty.QuickCheck (Arbitrary(..), Gen)

prop_Round_trip_for_optional_header_standard_and_windows_fields :: Fields -> Bool
prop_Round_trip_for_optional_header_standard_and_windows_fields fields =
  Cereal.decode (Cereal.encode fields) == Right fields

instance Arbitrary Fields where
  arbitrary = do
    standardFieldsMagic <- arbitrary
    standardFieldsMajorLinkerVersion <- arbitrary
    standardFieldsMinorLinkerVersion <- arbitrary
    standardFieldsSizeOfCode <- arbitrary
    standardFieldsSizeOfInitializedData <- arbitrary
    standardFieldsSizeOfUninitializedData <- arbitrary
    standardFieldsAddressOfEntryPoint <- arbitrary
    standardFieldsBaseOfCode <- arbitrary
    standardFieldsBaseOfData <- arbitraryOnPE32 standardFieldsMagic
    windowsFieldsImageBase <- arbitraryWord64ViaMagic standardFieldsMagic
    windowsFieldsSectionAlignment <- arbitrary
    windowsFieldsFileAlignment <- arbitrary
    windowsFieldsMajorOperatingSystemVersion <- arbitrary
    windowsFieldsMinorOperatingSystemVersion <- arbitrary
    windowsFieldsMajorImageVersion <- arbitrary
    windowsFieldsMinorImageVersion <- arbitrary
    windowsFieldsMajorSubsystemVersion <- arbitrary
    windowsFieldsMinorSubsystemVersion <- arbitrary
    windowsFieldsWin32VersionValue <- arbitrary
    windowsFieldsSizeOfImage <- arbitrary
    windowsFieldsSizeOfHeaders <- arbitrary
    windowsFieldsCheckSum <- arbitrary
    windowsFieldsSubsystem <- arbitrary
    windowsFieldsDllCharacteristics <- arbitrary
    windowsFieldsSizeOfStackReserve <- arbitraryWord64ViaMagic standardFieldsMagic
    windowsFieldsSizeOfStackCommit <- arbitraryWord64ViaMagic standardFieldsMagic
    windowsFieldsSizeOfHeapReserve <- arbitraryWord64ViaMagic standardFieldsMagic
    windowsFieldsSizeOfHeapCommit <- arbitraryWord64ViaMagic standardFieldsMagic
    windowsFieldsLoaderFlags <- arbitrary
    windowsFieldsNumberOfRvaAndSizes <- arbitrary
    pure . Fields StandardFields{..} $ WindowsFields{..}

arbitraryOnPE32 :: (Arbitrary a) => Magic -> Gen (Maybe a)
arbitraryOnPE32 PE32     = Just <$> arbitrary
arbitraryOnPE32 PE32Plus = pure Nothing

arbitraryWord64ViaMagic :: Magic -> Gen Word64
arbitraryWord64ViaMagic magic =
  arbitraryOnMagic magic (arbitrary :: Gen Word32) (arbitrary :: Gen Word64)

arbitraryOnMagic :: (Integral a, Integral b, Num c)
                 => Magic
                 -> Gen a
                 -> Gen b
                 -> Gen c
arbitraryOnMagic PE32     ga _  = fromIntegral <$> ga
arbitraryOnMagic PE32Plus _  gb = fromIntegral <$> gb
