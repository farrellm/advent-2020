module Advent2020.Day15 where

import qualified Data.IntMap as M
import Text.Megaparsec

step :: (Int, Int, IntMap Int) -> (Int, Int, IntMap Int)
step (i, p, m) =
  case m M.!? p of
    Just n -> (i + 1, i - n, M.insert p i m)
    Nothing -> (i + 1, 0, M.insert p i m)

part1 :: Int -> FilePath -> IO ()
part1 n f = do
  es <- unsafeParse (decimal `sepBy` ",") f
  let s0 = M.fromList $ fromList $ zip es [1 ..]
  print $ ((^. _2) <$> iterate step (length es + 1, 0, s0)) !! (n - 1 - length es)

step' :: Int -> (Int, Int, IntMap Int) -> Int
step' x (i, p, m)
  | x == i = p
  | otherwise =
    case m M.!? p of
      Just n -> step' x (i + 1, i - n, M.insert p i m)
      Nothing -> step' x (i + 1, 0, M.insert p i m)

part2 :: Int -> FilePath -> IO ()
part2 n f = do
  es <- unsafeParse (decimal `sepBy` ",") f
  let s0 = M.fromList $ fromList $ zip es [1 ..]
  print $ step' n (length es + 1, 0, s0)

test :: IO ()
test = do
  -- part1 2020 "data2020/test15.txt"
  -- part1 2020 "data2020/day15.txt"
  -- part2 2020 "data2020/test15.txt"
  -- part2 2020 "data2020/day15.txt"
  -- part2 30000000 "data2020/test15.txt"
  part1 30000000 "data2020/day15.txt"
  pass
