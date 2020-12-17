module Advent2020.Day16 where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (noneOf)

entry :: Parser ([(Text, ((Int, Int), (Int, Int)))], [Int], [[Int]])
entry = do
  rs <- rule `sepEndBy` eol
  eol *> "your ticket:" *> eol
  t <- tkt
  eol *> eol *> "nearby tickets:" *> eol
  ts <- tkt `sepBy` eol
  pure (rs, t, filter (not . null) ts)
 where
  rule = do
    n <- toText <$> many (noneOf [':', '\n'])
    ": "
    a <- rng
    " or "
    b <- rng
    pure (n, (a, b))
  rng = (,) <$> decimal <*> ("-" *> decimal)
  tkt = decimal `sepBy` ","

mkRule :: ((Int, Int), (Int, Int)) -> (Int -> Bool)
mkRule ((a, b), (x, y)) n = (a <= n && n <= b) || (x <= n && n <= y)

part1 :: FilePath -> IO ()
part1 f = do
  (rs, _, ts) <- unsafeParse entry f
  let rs' = second mkRule <$> rs
      es = filter (\k -> not (any ($ k) (snd <$> rs'))) <$> ts
  print $ sum (concat es)

findLabels :: Map Text [Int] -> Maybe ((Text, Int), Map Text [Int])
findLabels m =
  case filter ((== 1) . length . snd) $ M.toList m of
    (l, [x]) : _ -> Just ((l, x), filter (/= x) <$> m)
    _ -> Nothing

part2 :: FilePath -> IO ()
part2 f = do
  (rs, t, ts) <- unsafeParse entry f
  let rs' = second mkRule <$> rs
      ts' = filter (all (\k -> any ($ k) (snd <$> rs'))) ts
      ns = [0 .. length t - 1]
      m = (\p -> filter (\n -> all p $ (!! n) <$> ts') ns) <$> M.fromList rs'
      ls = fmap fst . sortOn snd $ unfoldr findLabels m
      z = product . fmap snd . filter (T.isPrefixOf "departure" . fst) $ zip ls t
  print z

test :: IO ()
test = do
  -- part1 "data2020/test16.txt"
  -- part1 "data2020/day16.txt"
  -- part2 "data2020/test16.txt"
  part2 "data2020/day16.txt"
  pass
