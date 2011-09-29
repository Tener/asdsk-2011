module Parser where

import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Word
import Control.Monad(when)
import Data.ByteString as BS
import qualified Data.Set as Set
import Data.Set (Set)

import DataTypes

parseInterests :: ByteString -> Either String Interests
parseInterests = parseOnly interestsParser

interestsParser :: Parser Interests
interestsParser = do
  b <- anyWord32be
  list <- many (takeWhile1 (/= 0))
  when (Prelude.length list /= fromIntegral b) (fail "Malformed packet: declared length is invalid")
  return (Set.fromList $ Prelude.map BS.unpack list)
