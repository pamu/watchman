module CommandLine
  ( commandLine
  , CommandLine(..)
  ) where

import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Model (Command, Port)
import Text.Parsec
import Text.Parsec.String

notSpace :: Parser Char
notSpace = satisfy (not . isSpace)

dirOption :: Parser ()
dirOption = do
  string "-d"
  spaces

portOption :: Parser ()
portOption = do
  string "-p"
  spaces

dir :: Parser FilePath
dir = do
  void dirOption
  result <- many1 notSpace
  void spaces
  return result

readInt :: String -> Int
readInt str = read str :: Int

port :: Parser Port
port = do
  void portOption
  result <- readInt <$> many1 (satisfy isDigit)
  void spaces
  return result

cmd :: Parser Command
cmd = string "server" <|> string "watch"

data CommandLine = CommandLine
  { _dir :: !(Maybe FilePath)
  , _port :: !(Maybe Port)
  , _cmd :: !Command
  } deriving (Show)

commandLine :: Parser CommandLine
commandLine =
  CommandLine <$> optionMaybe (try dir) <*> optionMaybe (try port) <*> cmd
