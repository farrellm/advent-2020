module Day5 where

import Data.Set ((\\))
import qualified Data.Set as S
import Relude.Extra (Foldable1 (maximum1))

decode :: String -> Int
decode = foldl' f 0
  where
    f a c
      | c == 'B' || c == 'R' = 2 * a + 1
      | otherwise = 2 * a

calcId :: (Int, Int) -> Int
calcId = uncurry (+) . first (* 8)

part1 :: IO ()
part1 = do
  ts <- lines <$> readFileText "data/day5.txt"
  let ps = bimap decode decode . splitAt 7 . toString <$> ts
      is = calcId <$> ps
  print $ viaNonEmpty maximum1 is

part2 :: IO ()
part2 = do
  ts <- lines <$> readFileText "data/day5.txt"
  let ps = S.fromList $ bimap decode decode . splitAt 7 . toString <$> ts
      allSeats = S.fromList $ do
        r <- [0 .. 127]
        c <- [0 .. 7]
        pure (r, c)
      missing = allSeats \\ ps
      good = do
        s@(r, c) <- S.toList missing
        guard $ S.member (r + 1, c) ps && S.member (r - 1, c) ps
        pure s
  traverse_ (print . calcId) good
