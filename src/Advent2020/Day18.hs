module Advent2020.Day18 where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Prelude hiding (Op)

data Expr
  = Plus Expr Expr
  | Mult Expr Expr
  | Term Int
  deriving (Show)

expr :: Parser Expr
expr = uni <**> (try bin <|> pure id)
 where
  uni = parens expr <|> Term <$> decimal <* many (char ' ')
  bin :: Parser (Expr -> Expr)
  bin = do
    f <- symbol "+" $> Plus <|> symbol "*" $> Mult
    x <- uni
    e <- bin <|> pure id
    pure (e . flip f x)

eval :: Expr -> Int
eval (Term n) = n
eval (Plus a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

part1 :: FilePath -> IO ()
part1 f = do
  ls <- unsafeParse (expr `sepEndBy` eol) f
  print $ sum (eval <$> ls)

data Expr' = Sub [Expr'] | Op Text | Tm Int
  deriving (Show)

expr' :: Parser Expr'
expr' = Sub <$> some ex
 where
  ex =
    parens expr'
      <|> Tm <$> decimal <* many (char ' ')
      <|> Op <$> (symbol "+" <|> symbol "*")

eval' :: Expr' -> Int
eval' (Tm n) = n
eval' (Sub es) = go es
 where
  go [e] = eval' e
  go [a, Op "*", b] = eval' a * eval' b
  go (a : Op "+" : b : rs) = go (Tm (eval' a + eval' b) : rs)
  go (a : Op "*" : rs) = eval' a * go rs

part2 :: FilePath -> IO ()
part2 f = do
  ls <- unsafeParse (expr' `sepEndBy` eol) f
  print $ sum (eval' <$> ls)

test :: IO ()
test = do
  -- part1 "data2020/test18.txt"
  -- part1 "data2020/day18.txt"
  -- part2 "data2020/test18.txt"
  part2 "data2020/day18.txt"
  pass
