module Advent2020.Day13 where

import Text.Megaparsec
import Text.Megaparsec.Char

entry :: Parser (Maybe Integer)
entry = "x" $> Nothing <|> Just <$> decimal

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge a@(x : xs) b@(y : ys)
  | x < y = x : merge xs b
  | otherwise = y : merge a ys

part1 :: FilePath -> IO ()
part1 f = do
  (t, es) <- unsafeParse ((,) <$> decimal <*> (eol *> entry `sepBy` ",")) f
  let xs = catMaybes es
      ss = (\x -> iterate (first (+ x)) (0, x)) <$> xs
      s = filter ((>= t) . fst) $ foldl' merge [] ss
      Just (d, i) = viaNonEmpty head s
  print ((d - t) * i)

findPattern :: Integer -> Integer -> [(Integer, Integer)] -> Integer
findPattern n _ [] = n
findPattern n d xs@((p, o) : xs')
  | (n + o) `mod` p == 0 = findPattern n (d * p) xs'
  | otherwise = findPattern (n + d) d xs

part2 :: FilePath -> IO ()
part2 f = do
  (_, es) <- unsafeParse ((,) <$> anySingleBut '\n' <*> (eol *> entry `sepBy` ",")) f
  let ps = zipWith (liftA2 (,)) es $ pure <$> [0 ..]
      t = findPattern 0 1 $ catMaybes ps
  print t

test :: IO ()
test = do
  -- part1 "data2020/test13.txt"
  -- part1 "data2020/day13.txt"
  -- part2 "data2020/test13.txt"
  part2 "data2020/day13.txt"
  pass
