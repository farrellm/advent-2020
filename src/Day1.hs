module Day1 where

part1 :: IO ()
part1 = do
  xs <- fmap (read . toString) . lines <$> readFileText "data/day1.txt" :: IO [Int]
  let ys = do
        a <- xs
        b <- xs
        guard (a > b)
        guard (a + b == 2020)
        pure (a * b)
  traverse_ print ys

part2 :: IO ()
part2 = do
  xs <- fmap (read . toString) . lines <$> readFileText "data/day1.txt" :: IO [Int]
  let ys = do
        a <- xs
        b <- xs
        c <- xs
        guard (a > b && b > c)
        guard (a + b + c == 2020)
        pure (a * b * c)
  traverse_ print ys
