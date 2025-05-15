{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Data.List (nub)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse (Parser f) s = f (Position 0 s)

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case parse p s of
                  Parsed x _ -> Just x
                  Failed _     -> Nothing

instance Functor Parser where

  fmap f (Parser ip) = Parser $ \input -> case ip input of
                                            Parsed x inp -> Parsed (f x) inp
                                            Failed errs  -> Failed errs

instance Applicative Parser where
  pure a = Parser (Parsed a)
  (Parser fab) <*> (Parser fa) =
    Parser $ \input -> case fab input of
                        Parsed f i0 -> case fa i0 of
                            Parsed g i1 -> Parsed (f g) i1
                            Failed e0  -> Failed e0
                        Failed e1  -> Failed e1


instance Alternative Parser where
  empty = Parser $ const $ Failed []
  -- Note: when both parsers fail, their errors are accumulated and *deduplicated* to simplify debugging
  (Parser f) <|> (Parser g) = Parser $ \input -> case f input of
                                              res@Parsed{}  -> res
                                              Failed err1   -> case g input of
                                                Parsed v1 i1 -> Parsed v1 i1
                                                Failed err2  -> Failed (nub (err1 ++ err2)) 

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser helper where
    helper (Position pos []) = Failed [Position pos EndOfInput]
    helper (Position pos (x:xs)) = case p x of
                      True -> Parsed x (Position (pos + 1) xs)
                      False -> Failed [Position pos (Unexpected x)]
