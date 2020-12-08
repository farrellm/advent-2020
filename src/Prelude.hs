{-# LANGUAGE TypeApplications #-}

module Prelude
  ( module X,
    Parser,
    unsafeParse,
    readInt,
    md5,
  )
where

import Control.Lens as X hiding (head1, last1, uncons, universe, (??))
import Crypto.Hash (MD5, hash)
import Data.Map as X ((!))
import Data.Set as X (intersection, union)
import Relude as X hiding (many, some)
import Relude.Extra.Enum as X
import Relude.Extra.Foldable1 as X
import Relude.Extra.Group as X
import Relude.Extra.Map as X
import Relude.Extra.Tuple as X
import Relude.Unsafe as X (read, (!!))
import Text.Megaparsec (Parsec, errorBundlePretty)
import qualified Text.Megaparsec as Mp
import Text.Megaparsec.Char.Lexer as X (binary, decimal, hexadecimal, octal, signed)

type Parser = Parsec Void Text

readInt :: (ToString s) => s -> Int
readInt = read . toString

unsafeParse :: (MonadIO m) => Parser a -> FilePath -> m a
unsafeParse p f = do
  mes <- Mp.parse p f <$> readFileText f
  case mes of
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right es -> pure es

md5 :: Text -> Text
md5 = show . hash @ByteString @MD5 . encodeUtf8
