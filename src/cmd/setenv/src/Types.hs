module Types where

data Environment = Environment { envName :: String
                               , envFile :: FilePath
                               } deriving (Show)

data Command = Load String
             | Choose
             | New String
             | Reset
             | Help
             deriving (Eq, Show)
