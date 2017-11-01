module AliasParser where

import Data.Char
import Control.Applicative

import Parser
import Alias

-- alias name="cd /path/"

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
  string "cd"
  spaces
  p <- pathP
  char '"'
  return $ Alias n p

toBash :: Alias -> String
toBash a =
  "alias " ++ name a ++ "=\"cd " ++ path a ++ "\""

fromBash :: String -> Maybe Alias
fromBash =
  runParser bashAliasP