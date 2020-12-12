module Advent2020.Day11 where

import Control.Comonad
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char

type Cell = Maybe Bool

data CA a = CA (Vector (Vector a)) (Int, Int)
  deriving (Functor, Eq)

instance Comonad CA where
  extract (CA g (r, c)) = g V.! r V.! c
  extend f (CA g (r, c)) =
    let g' =
          flip imap g $ \r' w ->
            flip imap w $ \c' _ ->
              f $ CA g (r', c')
     in CA g' (r, c)

cell :: Parser Cell
cell =
  "." $> Nothing
    <|> "L" $> Just False
    <|> "#" $> Just True

draw :: CA Cell -> IO ()
draw (CA g _) =
  for_ g $ \r -> do
    for_ r $ \case
      Nothing -> putStr "."
      Just True -> putStr "#"
      Just False -> putStr "L"
    putStrLn ""

step :: CA Cell -> Cell
step (CA g p@(r, c)) =
  let ss = mapMaybe (getCell . mv8 p) universe
      n = length $ filter id ss
   in g V.! r V.! c
        <&> if
            | n == 0 -> const True
            | n >= 4 -> const False
            | otherwise -> id
 where
  getCell (r', c') = join $ g V.!? r' >>= (V.!? c')

go :: (CA Cell -> Cell) -> CA Cell -> CA Cell
go s g =
  let g' = g =>> s
   in if g' == g
        then g
        else go s g'

part1 :: FilePath -> IO ()
part1 f = do
  es <- unsafeParse (some cell `sepEndBy` eol) f
  let d = fromList $ fromList <$> es
      g = CA d (0, 0)
      CA d' _ = go step g
      n = sum $ V.length . V.filter (== Just True) <$> d'
  print n

step' :: CA Cell -> Cell
step' (CA g p@(r, c)) =
  let ss = mapMaybe (\d -> firstSeat (mv8 p d) d) universe
      n = length $ filter id ss
   in g V.! r V.! c
        <&> if
            | n == 0 -> const True
            | n >= 5 -> const False
            | otherwise -> id
 where
  firstSeat :: (Int, Int) -> Dir8 -> Maybe Bool
  firstSeat q@(r', c') d
    | r' < 0 || c' < 0 || r' >= V.length g || c' >= V.length (g V.! r') = Nothing
    | otherwise =
      case g V.! r' V.! c' of
        Nothing -> firstSeat (mv8 q d) d
        Just b -> Just b

part2 :: FilePath -> IO ()
part2 f = do
  es <- unsafeParse (some cell `sepEndBy` eol) f
  let d = fromList $ fromList <$> es
      g = CA d (0, 0)
      CA d' _ = go step' g
      n = sum $ V.length . V.filter (== Just True) <$> d'
  print n

test :: IO ()
test = do
  -- part1 "data2020/test11.txt"
  part1 "data2020/day11.txt"
  -- part2 "data2020/test11.txt"
  part2 "data2020/day11.txt"
  pass
