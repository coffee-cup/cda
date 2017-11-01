{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char
import Control.Applicative
import Control.Monad

newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s ->
    [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser $ \s ->
    [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

runParser :: Parser a -> String -> Maybe a
runParser m s =
  case parse m s of
    [(res, [])] -> Just res
    [(_, _)]    -> Nothing
    _           -> Nothing

item :: Parser Char
item = Parser $ \s ->
  case s of
    []      -> []
    (c:cs)  -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s ->
  concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s ->
  parse p s ++ parse q s

failure :: Parser a
failure = Parser $ const []

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  if p c then unit c else failure

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

notOneOf :: String -> Parser Char
notOneOf s = satisfy (`notElem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = p `chainl1` op <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do  f <- op
                      b <- p
                      rest (f a b))
                <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

lower :: Parser Char
lower = satisfy (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = satisfy (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = lower <|> upper

digit :: Parser Char
digit = satisfy isDigit

alphanum :: Parser Char
alphanum = letter <|> digit

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

word :: Parser String
word = many letter

string :: String -> Parser String
string [] = return []
string (c:cs) = do {char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do {a <- p; spaces; return a}

symbol :: String -> Parser String
symbol s = token (string s)

identifier :: Parser String
identifier = many alphanum

spaces :: Parser String
spaces = many $ oneOf " \t"

number :: Parser Integer
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  symbol "("
  n <- m
  symbol ")"
  return n