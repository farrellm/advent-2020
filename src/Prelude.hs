{-# LANGUAGE TypeApplications #-}

module Prelude
  ( module X,
    Parser,
    Dir4 (..),
    Dir8 (..),
    rotLeft,
    rotRight,
    unsafeParse,
    readInt,
    md5,
    mv4,
    mv8,
    juxt,
    (#!),
    (#!!),
    (#!!!),
    (#!?),
    (#!!?),
    (#!!!?),
    printGrid,
    symbol,
    parens,
  )
where

import Control.Lens as X hiding (head1, last1, uncons, universe, (??))
import Crypto.Hash (MD5, hash)
import Data.List (maximum, minimum)
import Data.Map as X ((!))
import Data.Set as X (intersection, union)
import Data.Vector as X (Vector)
import qualified Data.Vector as V
import Relude as X hiding (many, some)
import Relude.Extra.Enum as X
import Relude.Extra.Foldable1 as X
import Relude.Extra.Group as X
import Relude.Extra.Map as X
import Relude.Extra.Tuple as X
import Relude.Unsafe as X (read, (!!))
import Text.Megaparsec (Parsec, errorBundlePretty)
import qualified Text.Megaparsec as Mp
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as X (binary, decimal, hexadecimal, octal, signed)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Dir4 = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)

data Dir8 = NN | NE | EE | SE | SS | SW | WW | NW
  deriving (Show, Eq, Ord, Enum, Bounded)

rotLeft :: Dir4 -> Dir4
rotLeft N = W
rotLeft W = S
rotLeft S = E
rotLeft E = N

rotRight :: Dir4 -> Dir4
rotRight N = E
rotRight E = S
rotRight S = W
rotRight W = N

readInt :: (ToString s) => s -> Int
readInt = read . toString

unsafeParse :: (MonadIO m) => Parser a -> FilePath -> m a
unsafeParse p f = do
  mes <- Mp.parse p f <$> readFileText f
  case mes of
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right es -> pure es

md5 :: Text -> Text
md5 = show . hash @ByteString @MD5 . encodeUtf8

mv4' :: (Enum a) => Dir4 -> (a, a) -> (a, a)
mv4' N = first pred
mv4' S = first succ
mv4' E = second succ
mv4' W = second pred

mv8' :: (Enum a) => Dir8 -> (a, a) -> (a, a)
mv8' NN = first pred
mv8' SS = first succ
mv8' EE = second succ
mv8' WW = second pred
mv8' NE = bimap pred succ
mv8' SE = bimap succ succ
mv8' NW = bimap pred pred
mv8' SW = bimap succ pred

mv4 :: (Enum a) => (a, a) -> Dir4 -> (a, a)
mv4 = flip mv4'

mv8 :: (Enum a) => (a, a) -> Dir8 -> (a, a)
mv8 = flip mv8'

juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt f g x = (f x, g x)

(#!) :: Vector a -> Int -> a
(#!) = (V.!)

(#!!) :: Vector (Vector a) -> (Int, Int) -> a
v #!! (a, b) = v V.! a V.! b

(#!!!) :: Vector (Vector (Vector a)) -> (Int, Int, Int) -> a
v #!!! (a, b, c) = v V.! a V.! b V.! c

(#!?) :: Vector a -> Int -> Maybe a
(#!?) = (V.!?)

(#!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
v #!!? (a, b) = v V.!? a >>= (V.!? b)

(#!!!?) :: Vector (Vector (Vector a)) -> (Int, Int, Int) -> Maybe a
v #!!!? (a, b, c) = v V.!? a >>= (V.!? b) >>= (V.!? c)

printGrid :: Map (Int, Int) Char -> IO ()
printGrid m = do
  let minR = minimum $ fst <$> keys m
      maxR = maximum $ fst <$> keys m
      minC = minimum $ snd <$> keys m
      maxC = minimum $ snd <$> keys m
  for_ [minR .. maxR] $ \r -> do
    for_ [minC .. maxC] $ \c ->
      case m !? (r, c) of
        Nothing -> putStr "."
        Just x -> putStr [x]
    putStrLn ""

sc :: Parser ()
sc = void $ Mp.many (char ' ')

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"
