module Advent2020.Day9 where

findError :: [Int] -> [Int] -> Maybe Int
findError p@(_ : ps) (r : rs) =
  if not (go p)
    then Just r
    else findError (ps ++ [r]) rs
 where
  go [] = False
  go (q : qs) = elem (r - q) qs || go qs
findError _ _ = Nothing

part1 :: FilePath -> IO ()
part1 f = do
  es <- fmap readInt . lines <$> readFileText f
  let (p, r) = splitAt 25 es
  print $ findError p r

findSeq :: Int -> [Int] -> [Int] -> [Int]
findSeq x ps ss@(s : ss') =
  case compare x (sum ps) of
    GT -> findSeq x (ps ++ [s]) ss'
    EQ -> ps
    LT -> findSeq x (drop 1 ps) ss
findSeq _ _ _ = []

part2 :: FilePath -> IO ()
part2 f = do
  es <- fmap readInt . lines <$> readFileText f
  let (p, r) = splitAt 25 es
      Just x = findError p r
      qs = findSeq x [] es
  print $ liftA2 (+) (viaNonEmpty minimum1 qs) (viaNonEmpty maximum1 qs)

test :: IO ()
test = do
  part1 "data2020/day9.txt"
  part2 "data2020/day9.txt"
