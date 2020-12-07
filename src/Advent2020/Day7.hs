module Advent2020.Day7 where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char

bag :: Parser Text
bag = do
  x <- toText <$> many (anySingleBut ' ')
  " "
  y <- toText <$> many (anySingleBut ' ')
  " bag"
  "s" <|> ""
  pure (x <> " " <> y)

nBag :: Parser (Text, Int)
nBag = do
  n <- decimal
  " "
  b <- bag
  pure (b, n)

entry :: Parser (Text, [(Text, Int)])
entry = do
  x <- bag
  " contain "
  ys <- ("no other bags" $> []) <|> (nBag `sepBy` ", ")
  "."
  pure (x, ys)

findIn :: [Text] -> [(Text, Set Text)] -> [Text]
findIn [] _ = []
findIn s m =
  let n = fst <$> filter (\(_, c) -> any (`member` c) s) m
   in n <> findIn n m

part1 :: FilePath -> IO ()
part1 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  let ss = second (S.fromList . (fst <$>)) <$> es
      gs = findIn ["shiny gold"] ss
  print . length $ ordNub gs

countInner :: Map Text Int -> [(Text, Int)] -> Int
countInner n ps = sum $ (\(c, i) -> i * (1 + n ! c)) <$> ps

part2 :: FilePath -> IO ()
part2 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  let m = M.fromList es
      n = countInner n <$> m
  print (n ! "shiny gold")

test :: IO ()
test = do
  part1 "data2020/day7.txt"
  part2 "data2020/day7.txt"
