module Advent2020.Day14 where

import Data.Bits
import Text.Megaparsec
import Text.Megaparsec.Char

data Entry = Mask [Maybe Bool] | Mem Int Int
  deriving (Show)

entry :: Parser Entry
entry =
  (Mask <$> ("mask = " *> many m))
    <|> (Mem <$> ("mem[" *> decimal) <*> ("] = " *> decimal))
 where
  m = "1" $> Just True <|> "0" $> Just False <|> "X" $> Nothing

masks :: [Maybe Bool] -> (Int, Int)
masks = foldl' (flip go) (0, 0)
 where
  go :: Maybe Bool -> (Int, Int) -> (Int, Int)
  go Nothing = bimap sh1 sh1
  go (Just True) = bimap (st1 . sh1) sh1
  go (Just False) = bimap sh1 (st1 . sh1)
  sh1 = (`shiftL` 1)
  st1 = (+ 1)

step :: ((Int, Int), IntMap Int) -> Entry -> ((Int, Int), IntMap Int)
step (_, m) (Mask l) = (masks l, m)
step (p@(s, c), m) (Mem i w) = (p, m & at i ?~ complement c .&. (s .|. w))

part1 :: FilePath -> IO ()
part1 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  let (_, m) = foldl' step ((0, 0), mempty) es
  print es
  print $ sum (elems m)

masks' :: Int -> [Maybe Bool] -> [Int]
masks' k = foldl' go [k] . zip [35, 34 ..]
 where
  go :: [Int] -> (Int, Maybe Bool) -> [Int]
  go ws (_, Just False) = ws
  go ws (i, Just True) = (.|. (1 `shiftL` i)) <$> ws
  go ws (i, Nothing) =
    liftA2 ($) [(.|. (1 `shiftL` i)), (.&. complement (1 `shiftL` i))] ws

step' :: ([Maybe Bool], IntMap Int) -> Entry -> ([Maybe Bool], IntMap Int)
step' (_, m) (Mask k) = (k, m)
step' (s, m) (Mem i x) =
  let ks = masks' i s
      m' = foldl' (\n k -> n & at k ?~ x) m ks
   in (s, m')

part2 :: FilePath -> IO ()
part2 f = do
  es <- unsafeParse (entry `sepEndBy` eol) f
  let m = foldl' step' ([], mempty) es
  print $ sum (elems $ snd m)

test :: IO ()
test = do
  -- part1 "data2020/test14.txt"
  -- part1 "data2020/day14.txt"
  -- part2 "data2020/test14-2.txt"
  part2 "data2020/day14.txt"
  pass
