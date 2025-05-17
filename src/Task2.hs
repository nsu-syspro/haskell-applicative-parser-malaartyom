{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

import Parser
import ParserCombinators (string, choice)
import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import GHC.Base (Alternative(many))

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year
  deriving (Show, Eq)

newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

monthNames :: [String]
monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

monthName :: Parser Month
monthName = choice
  [ Month 1  <$ string "Jan"
  , Month 2  <$ string "Feb"
  , Month 3  <$ string "Mar"
  , Month 4  <$ string "Apr"
  , Month 5  <$ string "May"
  , Month 6  <$ string "Jun"
  , Month 7  <$ string "Jul"
  , Month 8  <$ string "Aug"
  , Month 9  <$ string "Sep"
  , Month 10 <$ string "Oct"
  , Month 11 <$ string "Nov"
  , Month 12 <$ string "Dec"
  ]



nonZeroDigit :: Parser Int
nonZeroDigit = read <$> choice (string <$> ["1", "2", "3", "4", "5", "6", "7", "8",  "9"])


digit :: Parser Int
digit = (read <$> string "0") <|> nonZeroDigit

number :: Parser Int
number = read <$> (((++) . show <$> digit) <*> (concat <$> many (show <$> digit)))

year :: Parser Year
year = coerce number

month :: Parser Month
month = Month . read <$> ((++) <$> string "0"
  <*> (show <$> nonZeroDigit)
  <|> string "10"
  <|> string "11"
  <|> string "12")

day :: Parser Day
day = Day . read <$> choice
  [ (++) <$> string "0" <*> (show <$> nonZeroDigit)
  , (++) <$> string "1" <*> (show <$> digit)
  , (++) <$> string "2" <*> (show <$> digit)
  , string "30"
  , string "31"
  ]

usDay :: Parser Day
usDay = Day . read <$> choice [
  (++) <$> string "1" <*> (show <$> digit),
  (++) <$> string "2" <*> (show <$> digit),
  string "30",
  string "31",
  show <$> nonZeroDigit]

usFormat :: Parser Date
usFormat = do
  m <- monthName
  _ <- string " "
  d <- usDay
  _ <- string " "
  Date d m <$> year


hypenFormat :: Parser Date
hypenFormat = do
  d <- day
  _ <- string "-"
  m <- month
  _ <- string "-"
  Date d m <$> year



dotFormat :: Parser Date
dotFormat = do
  d <- day
  _ <- string "."
  m <- month
  _ <- string "."
  Date d m <$> year
-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDate ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-5050"
-- Parsed (Date (Day 12) (Month 12) (Year 5050)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012 )) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = choice [dotFormat, hypenFormat, usFormat]
