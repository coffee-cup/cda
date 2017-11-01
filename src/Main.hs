module Main where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Commands
import Parser
import Alias

doCommand :: Command -> AliasT ()
doCommand List = do
  aliases <- liftIO $ readAliasesFromFile filename
  liftIO $ forM_ aliases $ \a -> putStrLn (name a ++ " -> " ++ path a)
doCommand (Set n p) = do
  alias <- createAlias n p
  writeNewAlias filename alias
  liftIO $ putStrLn $ "Created alias " ++ name alias
doCommand (Delete n) = do
  aliases <- liftIO $ readAliasesFromFile filename
  newAliases <- deleteAlias n aliases
  liftIO $ writeAliases filename newAliases
  liftIO $ putStrLn $ "Deleted alias " ++ n

filename :: String
filename = "testing.txt"

main :: IO ()
main = do
  cmd <- getCommand
  ae <- runAliasT $ doCommand cmd
  either renderError return ae
