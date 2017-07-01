module Pep.Run
  ( main
  ) where

import Pep.PE

import qualified Control.Monad as Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Serialize as Cereal
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.IO as IO

main :: IO ()
main = Maybe.listToMaybe <$> Environment.getArgs >>= run
 where
  run Nothing = putStrLn "Usage: pep-dump.exe <exe_or_dll_file>"
  run (Just binaryPath) = iteM fileExists processFile (run Nothing)
   where
    fileExists = Directory.doesFileExist binaryPath
    processFile = do
      ePe <- decodePe <$> ByteString.readFile binaryPath
      case ePe of
        Left err -> IO.hPutStrLn IO.stderr err
        Right pe -> Monad.void (ByteString.writeFile "foo.exe" . Cereal.encode $ pe)

    decodePe :: ByteString -> Either String Pe
    decodePe = Cereal.decode

    iteM :: (Monad m) => m Bool -> m a -> m a -> m a
    iteM mb mt mf = mb >>= \b -> if b then mt else mf
