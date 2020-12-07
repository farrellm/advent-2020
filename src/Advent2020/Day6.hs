module Advent2020.Day6 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (group)

person :: Parser (Set Char)
person = fromList <$> some (anySingleBut '\n')

group :: Parser [Set Char]
group = person `sepEndBy` eol

part1 :: IO ()
part1 = do
  gs <- unsafeParse (group `sepEndBy` eol) "data2020/day6.txt"
  let es = length . mconcat <$> gs
  print $ sum es

part2 :: IO ()
part2 = do
  gs <- unsafeParse (group `sepEndBy` eol) "data2020/day6.txt"
  let es = length . foldl1' intersection <$> catMaybes (nonEmpty <$> gs)
  print $ sum es
