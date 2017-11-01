module Main where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Commands
import Parser
import Alias

doCommand :: Command -> IO ()
doCommand List = do
  aliases <- readAliasesFromFile filename
  forM_ aliases $ \a -> putStrLn (name a ++ " -> " ++ path a)
doCommand (Set n p) = do
  ae <- runAliasT $ trySet n p
  either renderError renderCreate ae
  where
    trySet :: String -> FilePath -> AliasT Alias
    trySet n p = do
      alias <- createAlias n p
      writeNewAlias alias filename
      return alias

    renderCreate :: Alias -> IO ()
    renderCreate alias = putStrLn $ "Created alias " ++ name alias

filename :: String
filename = "testing.txt"

main :: IO ()
main = do
  cmd <- getCommand
  doCommand cmd
