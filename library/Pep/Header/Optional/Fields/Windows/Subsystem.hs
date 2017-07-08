module Pep.Header.Optional.Fields.Windows.Subsystem
  ( Subsystem(..)
  , fromSubsystem
  , toSubsystem
  ) where

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word16)

data Subsystem = IMAGE_SUBSYSTEM_UNKNOWN
               | IMAGE_SUBSYSTEM_NATIVE
               | IMAGE_SUBSYSTEM_WINDOWS_GUI
               | IMAGE_SUBSYSTEM_WINDOWS_CUI
               | IMAGE_SUBSYSTEM_OS2_CUI
               | IMAGE_SUBSYSTEM_POSIX_CUI
               | IMAGE_SUBSYSTEM_NATIVE_WINDOWS
               | IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
               | IMAGE_SUBSYSTEM_EFI_APPLICATION
               | IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
               | IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
               | IMAGE_SUBSYSTEM_EFI_ROM
               | IMAGE_SUBSYSTEM_XBOX
               | IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION
               deriving (Bounded, Enum, Eq, Show)

instance Serialize Subsystem where
  get = do
    subsystem <- toSubsystem <$> Cereal.getWord16le
    maybe (fail "Subsystem in optional header's Windows fields is invalid") pure subsystem

  put = Cereal.putWord16le . fromSubsystem

fromSubsystem :: Subsystem -> Word16
fromSubsystem IMAGE_SUBSYSTEM_UNKNOWN                  =  0
fromSubsystem IMAGE_SUBSYSTEM_NATIVE                   =  1
fromSubsystem IMAGE_SUBSYSTEM_WINDOWS_GUI              =  2
fromSubsystem IMAGE_SUBSYSTEM_WINDOWS_CUI              =  3
fromSubsystem IMAGE_SUBSYSTEM_OS2_CUI                  =  5
fromSubsystem IMAGE_SUBSYSTEM_POSIX_CUI                =  7
fromSubsystem IMAGE_SUBSYSTEM_NATIVE_WINDOWS           =  8
fromSubsystem IMAGE_SUBSYSTEM_WINDOWS_CE_GUI           =  9
fromSubsystem IMAGE_SUBSYSTEM_EFI_APPLICATION          = 10
fromSubsystem IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER  = 11
fromSubsystem IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER       = 12
fromSubsystem IMAGE_SUBSYSTEM_EFI_ROM                  = 13
fromSubsystem IMAGE_SUBSYSTEM_XBOX                     = 14
fromSubsystem IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION = 15

toSubsystem :: Word16 -> Maybe Subsystem
toSubsystem  0 = Just IMAGE_SUBSYSTEM_UNKNOWN
toSubsystem  1 = Just IMAGE_SUBSYSTEM_NATIVE
toSubsystem  2 = Just IMAGE_SUBSYSTEM_WINDOWS_GUI
toSubsystem  3 = Just IMAGE_SUBSYSTEM_WINDOWS_CUI
toSubsystem  5 = Just IMAGE_SUBSYSTEM_OS2_CUI
toSubsystem  7 = Just IMAGE_SUBSYSTEM_POSIX_CUI
toSubsystem  8 = Just IMAGE_SUBSYSTEM_NATIVE_WINDOWS
toSubsystem  9 = Just IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
toSubsystem 10 = Just IMAGE_SUBSYSTEM_EFI_APPLICATION
toSubsystem 11 = Just IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
toSubsystem 12 = Just IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
toSubsystem 13 = Just IMAGE_SUBSYSTEM_EFI_ROM
toSubsystem 14 = Just IMAGE_SUBSYSTEM_XBOX
toSubsystem 15 = Just IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION
toSubsystem _  = Nothing
