module Main where

import Data.Monoid
import Options.Applicative
import Lib

data Command
  = List              -- List all aliases
  | Set String String -- Create a new alias
  | Delete String     -- Delete an alias
  | CD String         -- Change directories using an alias
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

parseCD :: Parser Command
parseCD = CD
  <$> argument str (metavar "NAME")

parseCommand :: Parser Command
parseCommand = subparser $
  command "list"  (parseList   `withInfo` "List cd aliases") <>
  command "set"   (parseSet    `withInfo` "Create cd alias") <>
  command "rm"    (parseDelete `withInfo` "Remove cd alias") <>
  command "cd"    (parseCD     `withInfo` "Change directories using an alias")

main :: IO ()
main = do
  cmd <- execParser (parseCommand `withInfo` "Create and manage cd aliases")
  print cmd