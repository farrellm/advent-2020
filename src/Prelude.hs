{-# LANGUAGE TypeApplications #-}

module Prelude
  ( module X,
    Parser,
    Dir4,
    Dir8,
    unsafeParse,
    readInt,
    md5,
    mv4,
    mv8,
    juxt,
    printGrid,
  )
where

import Control.Lens as X hiding (head1, last1, uncons, universe, (??))
import Crypto.Hash (MD5, hash)
import Data.List (maximum, minimum)
import Data.Map as X ((!))
import Data.Set as X (intersection, union)
import Relude as X hiding (many, some)
import Relude.Extra.Enum as X
import Relude.Extra.Foldable1 as X
import Relude.Extra.Group as X
import Relude.Extra.Map as X
import Relude.Extra.Tuple as X
import Relude.Unsafe as X (read, (!!))
import Text.Megaparsec (Parsec, errorBundlePretty)
import qualified Text.Megaparsec as Mp
import Text.Megaparsec.Char.Lexer as X (binary, decimal, hexadecimal, octal, signed)

type Parser = Parsec Void Text

data Dir4 = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)

data Dir8 = NN | NE | EE | SE | SS | SW | WW | NW
  deriving (Show, Eq, Ord, Enum, Bounded)

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

mv4 :: (Enum a) => Dir4 -> (a, a) -> (a, a)
mv4 N = first pred
mv4 S = first succ
mv4 E = second succ
mv4 W = second pred

mv8 :: Dir8 -> (Int, Int) -> (Int, Int)
mv8 NN = first pred
mv8 SS = first succ
mv8 EE = second succ
mv8 WW = second pred
mv8 NE = bimap pred succ
mv8 SE = bimap succ succ
mv8 NW = bimap pred pred
mv8 SW = bimap succ pred

juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt f g x = (f x, g x)

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
