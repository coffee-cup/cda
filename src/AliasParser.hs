module AliasParser where

import Data.Char
import Control.Applicative

import Parser
import Alias

-- alias name="/path/"

aliasIdent :: Parser String
aliasIdent =
  string "alias"

nameP :: Parser String
nameP = identifier

pathP :: Parser String
pathP =
  many $ notOneOf "\""

bashAliasP :: Parser Alias
bashAliasP = do
  aliasIdent
  spaces
  n <- nameP
  char '='
  char '"'
  p <- pathP
  char '"'
  return $ Alias n p

toBash :: Alias -> String
toBash a =
  "alias " ++ name a ++ "=\"" ++ path a ++ "\""

fromBash :: String -> Maybe Alias
fromBash =
  runParser bashAliasP