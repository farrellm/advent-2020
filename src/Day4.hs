module Day4 where

import qualified Data.Set as S
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

data Field
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving (Show, Eq)

field :: Parser Field
field =
  try "byr" $> BirthYear
    <|> try "iyr" $> IssueYear
    <|> try "eyr" $> ExpirationYear
    <|> try "hgt" $> Height
    <|> try "hcl" $> HairColor
    <|> try "ecl" $> EyeColor
    <|> try "pid" $> PassportID
    <|> "cid" $> CountryID

passport :: Parser [(Field, Text)]
passport =
  some
    ( (,) <$> field
        <*> (":" *> (toText <$> some (alphaNumChar <|> char '#')))
        <* (void (char ' ') <|> void eol)
    )

isValid :: [(Field, Text)] -> Bool
isValid = (== 7) . length . filter ((/= CountryID) . fst)

part1 :: IO ()
part1 = do
  mes <- parse (passport `sepEndBy` eol) "" <$> readFileText "data/day4.txt"
  case mes of
    Left err -> putStrLn $ errorBundlePretty err
    Right es -> print . length $ filter isValid es

isValidEntry :: (Field, Text) -> Bool
isValidEntry (BirthYear, t) =
  let y = readText t :: Int
   in 1920 <= y && y <= 2002
isValidEntry (IssueYear, t) =
  let y = readText t :: Int
   in 2010 <= y && y <= 2020
isValidEntry (ExpirationYear, t) =
  let y = readText t :: Int
   in 2020 <= y && y <= 2030
isValidEntry (Height, t) =
  let (a, b) = T.splitAt (T.length t - 2) t
      i = readText a :: Int
   in case b of
        "cm" -> 150 <= i && i <= 193
        "in" -> 59 <= i && i <= 76
        _ -> False
isValidEntry (HairColor, t)
  | T.length t == 7 =
    let c = "0x" <> T.drop 1 t
     in isJust (readMaybe (toString c) :: Maybe Int)
isValidEntry (EyeColor, t) = S.member t $ S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidEntry (PassportID, t)
  | T.length t == 9 =
    isJust (readMaybe (toString t) :: Maybe Int)
isValidEntry _ = False

part2 :: IO ()
part2 = do
  mes <- parse (passport `sepEndBy` eol) "" <$> readFileText "data/day4.txt"
  case mes of
    Left err -> putStrLn $ errorBundlePretty err
    Right es -> print . length . filter isValid $ filter isValidEntry <$> es
