module Advent2020.Day3 where

import qualified Data.String as S

part1 :: IO ()
part1 = do
  r <- S.lines <$> readFile "data/day3.txt"
  let bs = fmap (== '#') <$> r
      fs = cycle <$> bs
      ts = zipWith (!!) fs [0, 3 ..]
  print (length $ filter id ts)

part2 :: IO ()
part2 = do
  r <- S.lines <$> readFile "data/day3.txt"
  let bs = fmap (== '#') <$> r
      fs = cycle <$> bs
      n = length fs
      mkTs dr dc =
        let xs = bimap getSum getSum <$> iterate (<> (Sum dr, Sum dc)) mempty
         in (\(r, c) -> fs !! r !! c) <$> takeWhile ((< n) . fst) xs
      calc dr dc = length . filter id $ mkTs dr dc
  print (calc 1 1 * calc 1 3 * calc 1 5 * calc 1 7 * calc 2 1)
