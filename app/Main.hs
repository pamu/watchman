module Main where

import CommandLine (CommandLine(..), commandLine)

--------------------------------------------------------------------------------
import Commands (server, watch)
import Control.Monad (mfilter)
import Data.Maybe (fromMaybe)
import Logger (log)
import Model (Command, Config(..), Host, Port, defaultConfig)
import Prelude hiding (log)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs)
import Text.Parsec (parse)
import Text.Parsec.Error

--------------------------------------------------------------------------------
config :: [String] -> Config
config args = undefined

main :: IO ()
main = do
  cmdLine <- unwords <$> getArgs
  case (parse commandLine "Command line" cmdLine) of
    Left err -> do
      print err
      putStrLn ""
      log "Usage: watchman -d <Directory Path> -p <Port> watch|server"
    Right cmdLineParsed -> do
      currentDir <-
        case (_dir cmdLineParsed) of
          Nothing -> Just <$> getCurrentDirectory
          Just dir -> do
            result <- doesDirectoryExist dir
            if result
              then return $ Just dir
              else return Nothing
      case currentDir of
        Nothing -> putStrLn "Valid directory needed"
        Just validDir -> do
          let processedConfig = defaultConfig validDir
          let portChanged =
                (\x -> processedConfig {port = x}) <$> (_port cmdLineParsed)
          let config = fromMaybe processedConfig portChanged
          case (cmd config) of
            "watch" -> do
              log
                ("Watching directory " ++ (show $ dir config) ++ " for changes")
              watch (dir config) (host config) (port config) True
            "server" -> do
              log $ "Serving from directory " ++ (show $ dir config)
              server (dir config) (host config) (port config)
