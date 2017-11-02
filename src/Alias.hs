module Alias where

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