module Prelude
  ( module X,
    Parser,
  )
where

import Relude as X hiding (some)
import Relude.Unsafe as X (read, (!!))
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char.Lexer as X (binary, decimal, hexadecimal, octal, signed)

type Parser = Parsec Void Text
