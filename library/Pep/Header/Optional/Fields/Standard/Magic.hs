module Pep.Header.Optional.Fields.Standard.Magic
  ( Magic(..)
  , fromMagic
  , toMagic
  ) where

import Data.Serialize (Serialize)
import qualified Data.Serialize as Cereal
import Data.Word (Word16)

data Magic = PE32
           | PE32Plus
           deriving (Bounded, Enum, Eq, Show)

instance Serialize Magic where
  get = do
    magic <- toMagic <$> Cereal.getWord16le
    maybe (fail "Magic number in optional header is invalid") pure magic

  put = Cereal.putWord16le . fromMagic

fromMagic :: Magic -> Word16
fromMagic PE32     = 0x010b
fromMagic PE32Plus = 0x020b

toMagic :: Word16 -> Maybe Magic
toMagic 0x010b = Just PE32
toMagic 0x020b = Just PE32Plus
toMagic _      = Nothing
