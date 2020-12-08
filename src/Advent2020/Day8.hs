{-# LANGUAGE TypeApplications #-}

module Advent2020.Day8 where

import Control.Lens (element)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Relude.Unsafe as U
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
step (Nop _) = first succ
step (Acc n) = bimap succ (+ n)
step (Jmp n) = first (+ n)

go :: [Step] -> Set Int -> (Int, Int) -> (Int, Int)
go ss seen t
  | fst t `member` seen = t
  | otherwise = case ss ^? ix (fst t) of
    Nothing -> t
    Just s -> go ss (seen <> one (fst t)) $ step s t

part1 :: FilePath -> IO ()
part1 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  print es
  print $ go es mempty (0, 0)

alter :: Step -> Step
alter (Nop n) = Jmp n
alter a@(Acc _) = a
alter (Jmp n) = Nop n

alterNth :: [Step] -> Int -> [Step]
alterNth ss i = ss & ix i %~ alter

go' :: [Step] -> Set Int -> (Int, Int) -> Maybe (Int, Int)
go' ss seen t
  | fst t `member` seen = Nothing
  | otherwise = case ss ^? ix (fst t) of
    Nothing -> Just t
    Just s -> go' ss (seen <> one (fst t)) $ step s t

part2 :: FilePath -> IO ()
part2 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  let ps = alterNth es <$> [0 .. length es - 1]
  print $ catMaybes ((\p -> go' p mempty (0, 0)) <$> ps)

test :: IO ()
test = do
  -- part1 "data2020/test8.txt"
  -- part1 "data2020/day8.txt"
  -- part2 "data2020/test8-2.txt"
  part2 "data2020/day8.txt"
