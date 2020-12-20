module Advent2020.Day17 where

import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

cell :: Parser Bool
cell = "#" $> True <|> "." $> False

type V3 = (Int, Int, Int)

data S3 a = S3 V3 (Map V3 a)
  deriving (Show, Functor)

neighbors :: V3 -> [V3]
neighbors (x, y, z) = do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  dz <- [-1, 0, 1]
  guard (dx /= 0 || dy /= 0 || dz /= 0)
  [(x + dx, y + dy, z + dz)]

extend :: (S3 Bool -> Bool) -> S3 Bool -> S3 Bool
extend f (S3 i m) =
  let cs = keys m
      ns = neighbors =<< cs
      ps = ordNub (cs ++ ns)
      xs = (\p -> f $ S3 p m) <$> ps
   in S3 i . M.fromList $ filter snd (zip ps xs)

step :: S3 Bool -> Bool
step (S3 i m) =
  let ns = neighbors i
      xs = fromMaybe False . (m !?) <$> ns
      n = length $ filter id xs
   in case m !? i of
        Just True -> n == 2 || n == 3
        _ -> n == 3

part1 :: FilePath -> IO ()
part1 f = do
  es <- unsafeParse (many cell `sepEndBy` eol) f
  let s = S3 (0, 0, 0) $
        M.fromList $ do
          (x, r) <- zip [0 ..] es
          (y, c) <- zip [0 ..] r
          pure ((x, y, 0), c)
      S3 _ g' = iterate (extend step) s !! 6
  print $ M.size g'

type V4 = (Int, Int, Int, Int)

data S4 a = S4 V4 (Map V4 a)
  deriving (Show, Functor)

neighbors' :: V4 -> [V4]
neighbors' (x, y, z, w) = do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  dz <- [-1, 0, 1]
  dw <- [-1, 0, 1]
  guard (dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0)
  [(x + dx, y + dy, z + dz, w + dw)]

extend' :: (S4 Bool -> Bool) -> S4 Bool -> S4 Bool
extend' f (S4 i m) =
  let cs = keys m
      ns = neighbors' =<< cs
      ps = ordNub (cs ++ ns)
      xs = (\p -> f $ S4 p m) <$> ps
   in S4 i . M.fromList $ filter snd (zip ps xs)

step' :: S4 Bool -> Bool
step' (S4 i m) =
  let ns = neighbors' i
      xs = fromMaybe False . (m !?) <$> ns
      n = length $ filter id xs
   in case m !? i of
        Just True -> n == 2 || n == 3
        _ -> n == 3

part2 :: FilePath -> IO ()
part2 f = do
  es <- unsafeParse (many cell `sepEndBy` eol) f
  let s = S4 (0, 0, 0, 0) $
        M.fromList $ do
          (x, r) <- zip [0 ..] es
          (y, c) <- zip [0 ..] r
          pure ((x, y, 0, 0), c)
      S4 _ g' = iterate (extend' step') s !! 6
  print $ M.size g'

test :: IO ()
test = do
  -- part1 "data2020/test17.txt"
  -- part1 "data2020/day17.txt"
  -- part2 "data2020/test17.txt"
  part2 "data2020/day17.txt"
  pass
