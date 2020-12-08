{-# LANGUAGE TypeApplications #-}

module DayX where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Relude.Unsafe as U
import Text.Megaparsec
import Text.Megaparsec.Char

entry :: Parser ()
entry = pure ()

part1 :: FilePath -> IO ()
part1 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  pass

part2 :: FilePath -> IO ()
part2 f = do
  pass

test :: IO ()
test = do
  part1 "data20XX/testX.txt"
  -- part1 "data20XX/dayX.txt"
  -- part2 "data20XX/testX.txt"
  -- part2 "data20XX/dayX.txt"
  pass
