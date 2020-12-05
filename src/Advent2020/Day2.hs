module Advent2020.Day2 where

import Text.Megaparsec
import Text.Megaparsec.Char

data Entry = Entry
  { eMin :: Int,
    eMax :: Int,
    eChar :: Char,
    ePass :: String
  }
  deriving (Show)

entry :: Parser Entry
entry = do
  eMin <- decimal
  void "-"
  eMax <- decimal
  void " "
  eChar <- anySingle
  void ": "
  ePass <- some (anySingleBut '\n')
  pure Entry {..}

isValid :: Entry -> Bool
isValid Entry {..} =
  let n = length $ filter (== eChar) ePass
   in eMin <= n && n <= eMax

part1 :: IO ()
part1 = do
  mes <- parse (entry `sepEndBy` eol) "" <$> readFileText "data/day2.txt"
  case mes of
    Left err -> putStrLn $ errorBundlePretty err
    Right es -> print . length $ filter isValid es

isValid' :: Entry -> Bool
isValid' Entry {..} =
  let p = \i -> ePass !! (i - 1) == eChar
   in p eMin /= p eMax

part2 :: IO ()
part2 = do
  mes <- parse (entry `sepEndBy` eol) "" <$> readFileText "data/day2.txt"
  case mes of
    Left err -> putStrLn $ errorBundlePretty err
    Right es -> print . length $ filter isValid' es
