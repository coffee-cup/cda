module Main where

import Control.Monad
import Control.Monad.IO.Class

import Commands
import CommandParser
import Alias
import AliasIO

initString :: String
initString =
  "Alias file at `" ++ filename ++ "`. Please place \n\n\t \
  \ export -f cda () { command cda \"$@\"; source " ++ filename ++ "; } \n\n\
  \at then end of ~/.bash_profile or equivalent."

doCommand :: Command -> AliasT ()
doCommand List = do
  aliases <- liftIO $ readAliasesFromFile filename
  liftIO $ forM_ aliases $ \a -> print a
doCommand Init = do
  liftIO $ createFileIfNotExist filename
  liftIO $ putStrLn initString
doCommand (Set n p) = do
  alias <- createAlias n p
  writeNewAlias filename alias
  liftIO $ putStrLn $ "Created alias " ++ name alias
doCommand (Delete n) = do
  aliases <- liftIO $ readAliasesFromFile filename
  newAliases <- deleteAlias n aliases
  liftIO $ writeAliases filename newAliases
  liftIO $ putStrLn $ "Deleted alias " ++ n

filename :: FilePath
filename = "~/.cda"

main :: IO ()
main = do
  cmd <- getCommand
  ae <- runAliasT $ doCommand cmd
  either renderError return ae
