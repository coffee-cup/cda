module AliasIO where

import Prelude hiding (readFile)
import System.Directory
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.IO.Strict (readFile)
import System.FilePath.Posix (joinPath, splitPath, isValid)
import Data.Char
import Data.Maybe
import Control.Applicative ((<$>))

import Alias
import AliasParser

type AliasT a = ExceptT AliasError IO a

-- Pure

cdaHeader :: String
cdaHeader =
  "# CDA Alias File\n\
  \# --------------\n\n"

readAliases :: String -> [Alias]
readAliases s = fromJust <$> filter isJust maybeAliases
  where
    maybeAliases :: [Maybe Alias]
    maybeAliases = fmap fromBash (lines s)

aliasesToString :: [Alias] -> String
aliasesToString = foldr (\a acc -> toBash a ++ "\n" ++ acc) ""

verifyAliasUniquness :: Alias -> [Alias] -> Bool
verifyAliasUniquness = notElem

verifyAliasExist :: Name -> [Alias] -> Bool
verifyAliasExist n = verifyAliasUniquness (Alias n "")

-- IO

readAliasesFromFile :: FilePath -> IO [Alias]
readAliasesFromFile p = do
  p <- replaceHome p
  exist <- doesFileExist p
  if exist then do
    content <- readFile p
    return $ readAliases content
  else
    return []

createFileIfNotExist :: FilePath -> IO ()
createFileIfNotExist p = do
  p <- replaceHome p
  exist <- doesFileExist p
  unless exist $ writeFile p cdaHeader

writeAliases :: FilePath -> [Alias] -> IO ()
writeAliases p aliases = do
  p <- replaceHome p
  writeFile p s
  where
    s = cdaHeader ++ aliasesToString aliases

runAliasT :: AliasT a -> IO (Either AliasError a)
runAliasT = runExceptT

replaceHome :: FilePath -> IO FilePath
replaceHome p =
  case splitPath p of
    "~/" : t -> joinPath . (:t) <$> getHomeDirectory
    _ -> return p

renderError :: AliasError -> IO ()
renderError (PathDoesNotExist p)    = putStrLn ("Path `" ++ p ++ "` does not exist")
renderError (AliasAlreadyExists n)  = putStrLn ("Alias `" ++ n ++ "` already exists")
renderError (AliasDoesNotExist n)   = putStrLn ("Alias `" ++ n ++ "` does not exist")

-- AliasT

writeNewAlias :: FilePath -> Alias -> AliasT ()
writeNewAlias p a = do
  aliases <- liftIO $ readAliasesFromFile p
  unless (verifyAliasUniquness a aliases) (throwE (AliasAlreadyExists $ name a))
  liftIO $ writeAliases p (a : aliases)


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

createAlias :: FilePath -> Name -> AliasT Alias
createAlias n p = do
  expandedP <- verifyAndExpand p
  return $ Alias n expandedP

deleteAlias :: Name -> [Alias] -> AliasT [Alias]
deleteAlias n aliases = do
  when (verifyAliasExist n aliases) (throwE (AliasDoesNotExist n))
  return $ filter (\a -> name a /= n) aliases

getAliasPath :: Name -> [Alias] -> AliasT FilePath
getAliasPath n aliases = do
  when (verifyAliasExist n aliases) (throwE (AliasDoesNotExist n))
  return $ path $ head $ filter (\a -> name a == n) aliases
