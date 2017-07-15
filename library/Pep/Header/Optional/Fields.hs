{-# LANGUAGE RecordWildCards #-}

module Pep.Header.Optional.Fields
  ( Fields(..)
  ) where


import Pep.Header.Optional.Fields.Standard
import Pep.Header.Optional.Fields.Standard.Magic
import Pep.Header.Optional.Fields.Windows
import Pep.Header.Optional.Fields.Windows.Characteristics.DLL ()
import Pep.Header.Optional.Fields.Windows.Subsystem ()

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word64)

data Fields = Fields
  { standardFields :: !StandardFields
  , windowsFields  :: !WindowsFields
  } deriving (Eq, Show)

instance Serialize Fields where
  get = do
    standardFieldsMagic <- Cereal.get
    standardFieldsMajorLinkerVersion <- Cereal.getWord8
    standardFieldsMinorLinkerVersion <- Cereal.getWord8
    standardFieldsSizeOfCode <- Cereal.getWord32le
    standardFieldsSizeOfInitializedData <- Cereal.getWord32le
    standardFieldsSizeOfUninitializedData <- Cereal.getWord32le
    standardFieldsAddressOfEntryPoint <- Cereal.getWord32le
    standardFieldsBaseOfCode <- Cereal.getWord32le
    standardFieldsBaseOfData <- getOnPE32 standardFieldsMagic Cereal.getWord32le
    windowsFieldsImageBase <- getWord64ViaMagic standardFieldsMagic
    windowsFieldsSectionAlignment <- Cereal.getWord32le
    windowsFieldsFileAlignment <- Cereal.getWord32le
    windowsFieldsMajorOperatingSystemVersion <- Cereal.getWord16le
    windowsFieldsMinorOperatingSystemVersion <- Cereal.getWord16le
    windowsFieldsMajorImageVersion <- Cereal.getWord16le
    windowsFieldsMinorImageVersion <- Cereal.getWord16le
    windowsFieldsMajorSubsystemVersion <- Cereal.getWord16le
    windowsFieldsMinorSubsystemVersion <- Cereal.getWord16le
    windowsFieldsWin32VersionValue <- Cereal.getWord32le
    windowsFieldsSizeOfImage <- Cereal.getWord32le
    windowsFieldsSizeOfHeaders <- Cereal.getWord32le
    windowsFieldsCheckSum <- Cereal.getWord32le
    windowsFieldsSubsystem <- Cereal.get
    windowsFieldsDllCharacteristics <- Cereal.get
    windowsFieldsSizeOfStackReserve <- getWord64ViaMagic standardFieldsMagic
    windowsFieldsSizeOfStackCommit <- getWord64ViaMagic standardFieldsMagic
    windowsFieldsSizeOfHeapReserve <- getWord64ViaMagic standardFieldsMagic
    windowsFieldsSizeOfHeapCommit <- getWord64ViaMagic standardFieldsMagic
    windowsFieldsLoaderFlags <- Cereal.getWord32le
    windowsFieldsNumberOfRvaAndSizes <- Cereal.getWord32le
    pure . Fields StandardFields{..} $ WindowsFields{..}

  put (Fields StandardFields{..} WindowsFields{..}) = do
    Cereal.put standardFieldsMagic
    Cereal.putWord8 standardFieldsMajorLinkerVersion
    Cereal.putWord8 standardFieldsMinorLinkerVersion
    Cereal.putWord32le standardFieldsSizeOfCode
    Cereal.putWord32le standardFieldsSizeOfInitializedData
    Cereal.putWord32le standardFieldsSizeOfUninitializedData
    Cereal.putWord32le standardFieldsAddressOfEntryPoint
    Cereal.putWord32le standardFieldsBaseOfCode
    maybe (pure ()) Cereal.putWord32le standardFieldsBaseOfData
    putWord64ViaMagic standardFieldsMagic windowsFieldsImageBase
    Cereal.putWord32le windowsFieldsSectionAlignment
    Cereal.putWord32le windowsFieldsFileAlignment
    Cereal.putWord16le windowsFieldsMajorOperatingSystemVersion
    Cereal.putWord16le windowsFieldsMinorOperatingSystemVersion
    Cereal.putWord16le windowsFieldsMajorImageVersion
    Cereal.putWord16le windowsFieldsMinorImageVersion
    Cereal.putWord16le windowsFieldsMajorSubsystemVersion
    Cereal.putWord16le windowsFieldsMinorSubsystemVersion
    Cereal.putWord32le windowsFieldsWin32VersionValue
    Cereal.putWord32le windowsFieldsSizeOfImage
    Cereal.putWord32le windowsFieldsSizeOfHeaders
    Cereal.putWord32le windowsFieldsCheckSum
    Cereal.put windowsFieldsSubsystem
    Cereal.put windowsFieldsDllCharacteristics
    putWord64ViaMagic standardFieldsMagic windowsFieldsSizeOfStackReserve
    putWord64ViaMagic standardFieldsMagic windowsFieldsSizeOfStackCommit
    putWord64ViaMagic standardFieldsMagic windowsFieldsSizeOfHeapReserve
    putWord64ViaMagic standardFieldsMagic windowsFieldsSizeOfHeapCommit
    Cereal.putWord32le windowsFieldsLoaderFlags
    Cereal.putWord32le windowsFieldsNumberOfRvaAndSizes

getOnPE32 :: Magic -> Cereal.Get a -> Cereal.Get (Maybe a)
getOnPE32 PE32     g = Just <$> g
getOnPE32 PE32Plus _ = pure Nothing

getWord64ViaMagic :: Magic -> Cereal.Get Word64
getWord64ViaMagic magic = getOnMagic magic Cereal.getWord32le Cereal.getWord64le

getOnMagic :: (Integral a, Integral b, Num c)
           => Magic
           -> Cereal.Get a
           -> Cereal.Get b
           -> Cereal.Get c
getOnMagic PE32     ga _  = fromIntegral <$> ga
getOnMagic PE32Plus _  gb = fromIntegral <$> gb

putWord64ViaMagic :: Magic -> Word64 -> Cereal.Put
putWord64ViaMagic magic = putOnMagic magic Cereal.putWord32le Cereal.putWord64le

putOnMagic :: (Integral a, Integral b, Integral w)
           => Magic
           -> Cereal.Putter a
           -> Cereal.Putter b
           -> w
           -> Cereal.Put
putOnMagic PE32     pa _  value = pa $ fromIntegral value
putOnMagic PE32Plus _  pb value = pb $ fromIntegral value
