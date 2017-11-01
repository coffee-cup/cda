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
  show a = name a ++ " -> " ++ path a

instance Eq Alias where
  a1 == a2 = name a1 == name a2