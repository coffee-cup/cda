module Alias where

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

type Name = String

data Alias = Alias
  { name :: Name
  , path :: FilePath
  }

data AliasError
  = PathDoesNotExist FilePath
  | AliasAlreadyExists Name
  | AliasDoesNotExist Name
  deriving (Show)

instance Show Alias where
  show a = name a ++ ":" ++ path a

instance Eq Alias where
  a1 == a2 = name a1 == name a2

instance Read Alias where
  readsPrec _ input =
    let
      splitAtFirst x = fmap (drop 1) . break (x ==)
      (n, rest1) = splitAtFirst ':' input
      (p, rest2) = splitAtFirst '\n' rest1
    in
      [(Alias n p, rest2) | all isAlpha n && n /= "" && isValid p]

type AliasT a = ExceptT AliasError IO a

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

readAliases :: String -> [Alias]
readAliases s = fromJust <$> filter isJust maybeAliases
  where
    maybeAliases :: [Maybe Alias]
    maybeAliases = fmap maybeRead (lines s)

readAliasesFromFile :: FilePath -> IO [Alias]
readAliasesFromFile p = do
  exist <- doesFileExist p
  if exist then do
    content <- readFile p
    return $ readAliases content
  else
    return []

createFileIfNotExist :: FilePath -> IO ()
createFileIfNotExist p = do
  exist <- doesFileExist p
  unless exist $ writeFile p ""

aliasesToString :: [Alias] -> String
aliasesToString = foldr (\a acc -> show a ++ "\n" ++ acc) ""

writeNewAlias :: FilePath -> Alias -> AliasT ()
writeNewAlias p a = do
  aliases <- liftIO $ readAliasesFromFile p
  unless (verifyAliasUniquness a aliases) (throwE (AliasAlreadyExists $ name a))
  liftIO $ writeAliases p (a : aliases)

writeAliases :: FilePath -> [Alias] -> IO ()
writeAliases p = writeFile p . aliasesToString

verifyAliasUniquness :: Alias -> [Alias] -> Bool
verifyAliasUniquness = notElem

verifyAliasExist :: Name -> [Alias] -> Bool
verifyAliasExist n = verifyAliasUniquness (Alias n "")

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
renderError (PathDoesNotExist p)    = putStrLn ("Path `" ++ p ++ "` does not exist")
renderError (AliasAlreadyExists n)  = putStrLn ("Alias `" ++ n ++ "` already exists")
renderError (AliasDoesNotExist n)   = putStrLn ("Alias `" ++ n ++ "` does not exist")

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

runAliasT :: AliasT a -> IO (Either AliasError a)
runAliasT = runExceptT