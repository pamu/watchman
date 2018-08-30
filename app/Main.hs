module Main where

import Commands (server, watch)
import Logger (Logger)
import qualified Logger as Logger
import System.Directory
import System.Environment

main :: IO ()
main = do
  logger <- Logger.new Logger.Message
  args <- getArgs
  dir <- getCurrentDirectory
  let currentDir = dir
  let host = "127.0.0.1"
  let port = 8000
  case args of
    ("watch":_) -> do
      _ <-
        Logger.header logger $
        ("Watching directory " ++ show currentDir ++ " for changes")
      watch currentDir logger host port True
    ("server":_) -> do
      server currentDir logger host port
      Logger.header logger $ "Serving from directory " ++ show currentDir
    _ -> Logger.header logger "Usage: watchman watch|server"
  Logger.flush logger
