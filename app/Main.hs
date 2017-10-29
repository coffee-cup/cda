module Main where

import Data.Monoid
import Options.Applicative
import Lib

data Command
  = List
  | Set String String
  | Delete String
  deriving (Show)

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = info (helper <*> p) $ progDesc desc

parseList :: Parser Command
parseList = pure List

parseSet :: Parser Command
parseSet = Set
  <$> argument str (metavar "NAME")
  <*> argument str (metavar "DIR")

parseDelete :: Parser Command
parseDelete = Delete
  <$> argument str (metavar "NAME")

parseCommand :: Parser Command
parseCommand = subparser $
  command "list"  (parseList   `withInfo` "List cd aliases") <>
  command "set"   (parseSet    `withInfo` "Create cd alias") <>
  command "rm"    (parseDelete `withInfo` "Remove cd alias")


main :: IO ()
main = do
  cmd <- execParser (parseCommand `withInfo` "Create cd aliases")
  print cmd