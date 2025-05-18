{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module ParserCombinators where

import Parser

import Control.Applicative
import Data.Functor (void)

-- | Parses single character
--
-- Usage example:
--
-- >>> parse (char 'b') "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (char 'b') "abc"
-- Failed [Position 0 (Unexpected 'a')]
--
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Parses given string
--
-- Usage example:
--
-- >>> parse (string "ba") "bar"
-- Parsed "ba" (Position 2 "r")
-- >>> parse (string "ba") "abc"
-- Failed [Position 0 (Unexpected 'a')]
--
string :: String -> Parser String
string ""     = pure ""
string (x:xs) = (:) <$> char x <*> string xs

-- | Skips zero or more space characters
--
-- Usage example:
--
-- >>> parse spaces "  bar"
-- Parsed () (Position 2 "bar")
-- >>> parse spaces "bar"
-- Parsed () (Position 0 "bar")
-- >>> parse (spaces *> string "bar") "bar"
-- Parsed "bar" (Position 3 "")
--
spaces :: Parser ()
spaces = void $ many (satisfy (== ' '))

-- | Tries to consecutively apply each of given list of parsers until one succeeds.
-- Returns the *first* succeeding parser as result or 'empty' if all of them failed.
--
-- Usage example:
--
-- >>> parse (choice [char 'a', char 'b']) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (choice [char 'a', char 'b']) "foo"
-- Failed [Position 0 (Unexpected 'f')]
-- >>> parse (choice [string "ba", string "bar"]) "bar"
-- Parsed "ba" (Position 2 "r")
--
choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = asum



-- | Parses one character that is in the given list
oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- Discover and implement more useful parser combinators below
--
-- - <https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/Control-Applicative-Combinators.html>
-- - <https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Char.html>
