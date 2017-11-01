module Commands where

data Command
  = List              -- List all aliases
  | Set String String -- Create a new alias
  | Delete String     -- Delete an alias
  | CD String         -- Change directories using an alias
  deriving (Show)