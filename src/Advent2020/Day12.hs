module Advent2020.Day12 where

import Text.Megaparsec
import Text.Megaparsec.Char

data Act = Abs Dir4 | L | R | F
  deriving (Show)

act :: Parser Act
act =
  "N" $> Abs N
    <|> "S" $> Abs S
    <|> "E" $> Abs E
    <|> "W" $> Abs W
    <|> "L" $> L
    <|> "R" $> R
    <|> "F" $> F

entry :: Parser (Act, Int)
entry = (,) <$> act <*> decimal

mv :: (Dir4, (Int, Int)) -> (Act, Int) -> (Dir4, (Int, Int))
mv p (_, 0) = p
mv (d, p) (a, n) =
  case a of
    Abs e -> mv (d, mv4 p e) (a, n - 1)
    L -> mv (rotLeft d, p) (a, n - 90)
    R -> mv (rotRight d, p) (a, n - 90)
    F -> mv (d, mv4 p d) (a, n - 1)

part1 :: FilePath -> IO ()
part1 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  let (_, (r, c)) = foldl' mv (E, (0, 0)) es
  print (abs r + abs c)

mv' :: ((Int, Int), (Int, Int)) -> (Act, Int) -> ((Int, Int), (Int, Int))
mv' p (_, 0) = p
mv' (p, w) (a, n) =
  case a of
    Abs e -> mv' (p, mv4 w e) (a, n - 1)
    L -> mv' (p, rl w) (a, n - 90)
    R -> mv' (p, rr w) (a, n - 90)
    F -> mv' (add p w, w) (a, n - 1)
 where
  rl (s, e) = (- e, s)
  rr (s, e) = (e, - s)

  add (u, v) (x, y) = (u + x, v + y)

part2 :: FilePath -> IO ()
part2 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  let ((r, c), _) = foldl' mv' ((0, 0), (-1, 10)) es
  print (abs r + abs c)

test :: IO ()
test = do
  -- part1 "data2020/test12.txt"
  part1 "data2020/day12.txt"
  -- part2 "data2020/test12.txt"
  part2 "data2020/day12.txt"
  pass
