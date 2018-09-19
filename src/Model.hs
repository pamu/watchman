module Model
  ( Config(..)
  , defaultConfig
  , Port
  , Command
  , Host
  ) where

type Port = Int

type Command = String

type Host = String

data Config = Config
  { dir :: !FilePath
  , host :: !Host
  , port :: !Port
  , cmd :: !Command
  } deriving (Show)

defaultConfig :: FilePath -> Config
defaultConfig dir =
  Config {dir = dir, host = "127.0.0.1", port = 8000, cmd = "server"}
