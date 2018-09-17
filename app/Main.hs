module Main where

import Commands (server, watch)
import Logger (Logger)
import qualified Logger
import Model
import System.Directory
import System.Environment

data Config = Config
  { dir :: FilePath
  , host :: Host
  , port :: Port
  , cmd :: Command
  } deriving (Show)

defaultConfig :: FilePath -> Config
defaultConfig dir =
  Config {dir = dir, host = "127.0.0.1", port = 8000, cmd = "server"}

config :: [String] -> Config
config _ = undefined

main :: IO ()
main = do
  logger <- Logger.new Logger.Message
  args <- getArgs
  currentDir <- getCurrentDirectory
  let config = defaultConfig currentDir
  case args of
    ("watch":_) -> do
      Logger.header
        logger
        ("Watching directory " ++ show currentDir ++ " for changes")
      watch currentDir logger (host config) (port config) True
    ("server":_) -> do
      server currentDir logger (host config) (port config)
      Logger.header logger $ "Serving from directory " ++ show currentDir
      Logger.header
        logger
        "Usage: watchman -d <Directory Path> -p <Port> watch|server"
  Logger.flush logger
