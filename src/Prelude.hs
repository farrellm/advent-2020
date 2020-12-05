module Prelude
  ( module X,
    Parser,
    unsafeParse,
    readInt,
  )
where

import Data.Map as X ((!))
import Relude as X hiding (many, some)
import Relude.Extra.Foldable1 as X
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
