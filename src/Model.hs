module Model where

type Port = Int

type Command = String

type Host = String

newtype CommandLineArgs = CommandLineArgs
  { args :: [String]
  } deriving (Show)
