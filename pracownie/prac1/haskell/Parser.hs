module Parser where

import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Word
import Control.Monad(when)
import Data.ByteString as BS
import qualified Data.Set as Set
import Data.Set (Set)

import DataTypes
import Text.Printf

parseInterests :: ByteString -> Either String Interests
parseInterests = parseOnly interestsParser

interestsParser :: Parser Interests
interestsParser = do
  b <- anyWord32be
  list <- many (do
                  str <- takeWhile1 (/= 0)
                  anyWord8
                  return str
                )
  let len = Prelude.length list
  when (fromIntegral b /= len) (fail $ printf "Malformed packet: declared length is invalid. WAS: %d DECLARED: %d DATA=%s"
                                  b len (show list))
  return (Set.fromList $ Prelude.map BS.unpack list)
