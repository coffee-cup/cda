module CommandParser where

import Data.Monoid
import Options.Applicative
import Commands

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = info (helper <*> p) $ progDesc desc

parseList :: Parser Command
parseList = pure List

parseInit :: Parser Command
parseInit = pure Init

parseSet :: Parser Command
parseSet = Set
  <$> argument str (metavar "NAME")
  <*> argument str (metavar "DIR")

parseDelete :: Parser Command
parseDelete = Delete
  <$> argument str (metavar "NAME")

parseCommand :: Parser Command
parseCommand = subparser $
  command "list"  (parseList    `withInfo` "List cd aliases")         <>
  command "init"  (parseInit    `withInfo` "Initializes alias file")  <>
  command "set"   (parseSet     `withInfo` "Create cd alias")         <>
  command "rm"    (parseDelete  `withInfo` "Remove cd alias")

getCommand :: IO Command
getCommand =
  execParser (parseCommand `withInfo` "Create and manage cd aliases")