module Alias where

import System.Directory
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.FilePath.Posix (joinPath, splitPath, isValid)
import Data.Char

data Alias = Alias
  { name :: String
  , path :: FilePath
  }

data AliasError
  = PathDoesNotExist FilePath
  deriving (Show)

instance Show Alias where
  show a = name a ++ ":" ++ path a

instance Read Alias where
  readsPrec _ input =
    let
      splitAtFirst x = fmap (drop 1) . break (x ==)
      (n, rest1) = splitAtFirst ':' input
      (p, rest2) = splitAtFirst '\n' rest1
    in
      [(Alias n p, rest2) | all isAlpha n && n /= "" && isValid p]


type AliasT a = ExceptT AliasError IO a

replaceHome :: FilePath -> IO FilePath
replaceHome p =
  case splitPath p of
    "~/" : t -> joinPath . (:t) <$> getHomeDirectory
    _ -> return p

verifyDirectory :: FilePath -> AliasT FilePath
verifyDirectory p = either throwE return =<< liftIO (safeDirectoryExist p)
  where
    safeDirectoryExist :: FilePath -> IO (Either AliasError FilePath)
    safeDirectoryExist p = do
      exist <- doesDirectoryExist p
      return $ if exist then Right p else Left (PathDoesNotExist p)

verifyAndExpand :: FilePath -> AliasT FilePath
verifyAndExpand p = do
  expandedP <- liftIO $ replaceHome p
  verifiedP <- verifyDirectory expandedP
  liftIO $ makeAbsolute verifiedP

renderError :: AliasError -> IO ()
renderError (PathDoesNotExist p) = putStrLn ("Path `" ++ p ++ "` does not exist")

runAliasT :: AliasT a -> IO (Either AliasError a)
runAliasT = runExceptT