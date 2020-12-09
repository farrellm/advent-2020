module Advent2020.Day8 where

import qualified Data.IntSet as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (alter)

data Step = Nop Int | Acc Int | Jmp Int
  deriving (Show)

entry :: Parser Step
entry =
  "nop " *> (Nop <$> signed pass decimal)
    <|> "acc " *> (Acc <$> signed pass decimal)
    <|> "jmp " *> (Jmp <$> signed pass decimal)

step :: Step -> (Int, Int) -> (Int, Int)
step (Nop _) = first (+ 1)
step (Acc n) = bimap (+ 1) (+ n)
step (Jmp n) = first (+ n)

go :: Vector Step -> IntSet -> (Int, Int) -> (Int, Int)
go ss seen t@(i, _)
  | i `member` seen = t
  | otherwise = case ss ^? ix i of
    Nothing -> t
    Just s -> go ss (S.insert i seen) $ step s t

part1 :: FilePath -> IO ()
part1 f = do
  es <- fromList <$> unsafeParse (entry `sepEndBy` eol) f
  print $ go es mempty (0, 0)

alter :: Step -> Step
alter (Nop n) = Jmp n
alter a@(Acc _) = a
alter (Jmp n) = Nop n

alterNth :: Vector Step -> Int -> Vector Step
alterNth ss i = ss & ix i %~ alter

go' :: Vector Step -> IntSet -> (Int, Int) -> Maybe (Int, Int)
go' ss seen t@(i, _)
  | i `member` seen = Nothing
  | otherwise = case ss ^? ix i of
    Nothing -> Just t
    Just s -> go' ss (S.insert i seen) $ step s t

part2 :: FilePath -> IO ()
part2 f = do
  es <- fromList <$> unsafeParse (entry `sepEndBy` eol) f
  let ps = alterNth es <$> [0 .. length es - 1]
  print $ catMaybes ((\p -> go' p mempty (0, 0)) <$> ps)

test :: IO ()
test = do
  -- part1 "data2020/test8.txt"
  part1 "data2020/day8.txt"
  -- part2 "data2020/test8-2.txt"
  part2 "data2020/day8.txt"
