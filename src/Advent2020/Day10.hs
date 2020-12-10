module Advent2020.Day10 where

import qualified Data.Map as M
import qualified Data.Set as S

diffs :: [Int] -> [Int]
diffs (x : xs@(y : _)) = (y - x) : diffs xs
diffs _ = []

part1 :: FilePath -> IO ()
part1 f = do
  es <- fmap readInt . lines <$> readFileText f
  let ds = diffs $ sort (0 : es)
  print $ length (filter (== 1) ds) * (1 + length (filter (== 3) ds))

countSeqs :: Map Int Int -> [Int] -> Int -> Int
countSeqs m xs j =
  let ys = filter (liftA2 (&&) (> j) (<= j + 3)) xs
   in if null ys
        then 1
        else sum ((m !) <$> ys)

part2 :: FilePath -> IO ()
part2 f = do
  es <- fmap readInt . lines <$> readFileText f
  let m = M.fromSet (countSeqs m es) $ S.fromList (0 : es)
  print $ m ! 0

test :: IO ()
test = do
  -- part1 "data2020/test10-2.txt"
  part1 "data2020/day10.txt"
  -- part2 "data2020/test10.txt"
  -- part2 "data2020/test10-2.txt"
  part2 "data2020/day10.txt"
  pass
