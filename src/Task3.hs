{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import Data.Char (toLower)
import Data.List (intercalate)
import ParserCombinators (char, string, choice, oneOf)
import Control.Applicative ((<|>), Alternative (..))
import Prelude hiding (exponent)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)


pc2s :: Parser Char -> Parser String
pc2s p = (:[]) <$> p


value :: Parser JValue
value = choice [object, array, JString <$> jstring, number, jbool, jnull]

jnull :: Parser JValue
jnull = JNull <$ string "null"


jbool :: Parser JValue
jbool = JBool . ps2b <$> (string "true" <|> string "false") where
  ps2b s = case s of
    "true"  -> True
    "false" -> False
    _       -> error "Unexpected boolean string"

object :: Parser JValue
object = do
  _ <- char '{'
  _ <- ws
  obj <- members
  _ <- ws
  _ <- char '}'
  pure $ JObject obj

members :: Parser [(String, JValue)]
members = (do
  _  <- ws
  m  <- member
  ms <- many $ do
    _ <- ws
    _ <- char ','
    _ <- ws
    member
  pure (m:ms))
  <|> pure []

member :: Parser (String, JValue)
member = do
  _ <- ws
  s <- jstring
  _ <- ws
  _ <- char ':'
  e <- element
  pure (s, e)

array :: Parser JValue
array = do
  _ <- char '['
  _ <- ws
  v <- elements
  _ <- ws
  _ <- char ']'
  pure $ JArray v

elements :: Parser [JValue]
elements = (do
  e <- element
  es <- many $ do
    _ <- ws
    _ <- char ','
    _ <- ws
    element
  pure (e : es))
  <|> pure []


element :: Parser JValue
element = do
    _ <- ws
    v <- value
    _ <- ws
    pure v


jstring :: Parser String
jstring = do
  _ <- pc2s $ char '"'
  c <- characters
  _ <- pc2s $ char '"'
  pure c

characters :: Parser String
characters = concat <$> many character


character :: Parser String
character = 
  pc2s (satisfy allowed) 
  <|> (do 
        _ <- char '\\'
        e <- escape
        pure ('\\' : e)
      )
  where 
    allowed c = c >= ' ' && c <= '\x10FFFF' && c /= '"' && c /= '\\'

escape :: Parser String
escape = escapeChar <|> unicodeChar where 

  escapeChar :: Parser String
  escapeChar = do
    c <- oneOf ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']
    pure [c]


  unicodeChar :: Parser String
  unicodeChar = do
    _ <- char 'u'
    h1 <- hex
    h2 <- hex
    h3 <- hex
    h4 <- hex
    pure $ "u" ++ [h1, h2, h3, h4]


hex :: Parser Char
hex = digit
  <|> choice (char <$> ['A'..'F'])
  <|> choice (char <$> ['a'..'f'])


number :: Parser JValue
number = do
  i <- integer
  f <- fraction
  e <- exponent
  pure $ JNumber $ read $ i <> f <> e

exponent :: Parser String
exponent = be <|> se <|> string "" where
  be = do
    e <- string "E"
    s <- sign
    d <- digits
    pure $ e <> s <> d
  se = do
    e <- string "e"
    s <- sign
    d <- digits
    pure $ e <> s <> d

fraction :: Parser String
fraction = (<>) <$> string "." <*> digits <|> string ""

integer :: Parser String
integer =
  (<>) <$> pc2s onenine <*> digits
  <|> do
    m <- string "-"
    o <- pc2s onenine
    d <- digits
    pure $ m <> o <> d
  <|> pc2s digit
  <|> (<>) <$>  string "-" <*> pc2s digit -- <|> (<>) <$> looks like i'm using drugs of smth  

digits :: Parser String
digits = some digit

digit :: Parser Char
digit = char  '0' <|> onenine

onenine :: Parser Char
onenine = choice $  char  <$> ['1'..'9']

sign :: Parser String
sign = string "+" <|> string "-" <|> string ""

ws :: Parser String
ws = some (satisfy isWs) <|> string "" where
  isWs c = c == ' ' || c == '\n' || c == '\r' || c == '\t'


-- | Parses JSON value-
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = element

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
