module Commands where

data Command
  = List              -- List all aliases
  | Init              -- Create empty alias file
  | Set String String -- Create a new alias
  | Delete String     -- Delete an alias
  deriving (Show)