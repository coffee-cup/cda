module Alias where

import System.Directory
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.FilePath.Posix (joinPath, splitPath)

import qualified Control.Exception as E

data Alias = Alias
  { name :: String
  , path :: FilePath
  }
  deriving (Show)

data AliasError
  = PathDoesNotExist
  deriving (Show)

type AliasT a = ExceptT AliasError IO a

verifyDirectory :: FilePath -> AliasT FilePath
verifyDirectory p = either throwE return =<< liftIO (safeDirectoryExist p)
  where
    safeDirectoryExist :: FilePath -> IO (Either AliasError FilePath)
    safeDirectoryExist p = do
      exist <- doesDirectoryExist p
      return $ if exist then Right p else Left PathDoesNotExist

expandPath :: FilePath -> IO FilePath
expandPath p =
  case splitPath p of
    "~/" : t -> joinPath . (:t) <$> getHomeDirectory
    _ -> return p

verifyAndExpand :: FilePath -> AliasT FilePath
verifyAndExpand p = do
  expandedP <- liftIO $ expandPath p
  verifiedP <- verifyDirectory expandedP
  liftIO $ makeAbsolute verifiedP

renderError :: AliasError -> IO ()
renderError e = do
  print e
  return ()

runAliasT :: AliasT a -> IO (Either AliasError a)
runAliasT = runExceptT