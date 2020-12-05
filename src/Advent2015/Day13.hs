module Advent2015.Day13 where

import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char

entry :: Parser (Text, Text, Int)
entry = do
  a <- toText <$> some (anySingleBut ' ')
  " would "
  sgn <- "gain" $> 1 <|> "lose" $> -1
  " "
  n <- decimal
  " happiness units by sitting next to "
  b <- toText <$> some (anySingleBut '.')
  "."
  pure (a, b, sgn * n)

neighbors :: [a] -> [(a, a)]
neighbors rs = go (rs ++ take 1 rs)
 where
  go (a : rs@(b : _)) = (a, b) : go rs
  go _ = []

part1 :: IO ()
part1 = do
  es <- unsafeParse (entry `sepEndBy` eol) "./data2015/day13.txt"
  let (n : ns) = sortNub $ es ^.. traverse . _1
      c =
        foldl'
          (\c (a, b, i) -> c & at a . non mempty . at b ?~ i)
          mempty
          es ::
          Map Text (Map Text Int)
      us = do
        ns' <- permutations ns
        let ps = neighbors (n : ns')
            u = sum $ (\(a, b) -> [c ! a ! b, c ! b ! a]) =<< ps
        pure u
  print $ viaNonEmpty maximum1 us

part2 :: IO ()
part2 = do
  es <- unsafeParse (entry `sepEndBy` eol) "./data2015/day13.txt"
  let ns = sortNub $ es ^.. traverse . _1
      c =
        foldl'
          (\c (a, b, i) -> c & at a . non mempty . at b ?~ i)
          mempty
          es ::
          Map Text (Map Text Int)
      foo a b = c ^. at a . non mempty . at b
      us = do
        ns' <- permutations ns
        let ps = neighbors ("You" : ns')
            u = sum $ (\(a, b) -> catMaybes [foo a b, foo b a]) =<< ps
        pure u
  print $ viaNonEmpty maximum1 us
