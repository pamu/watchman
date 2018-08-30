--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Commands
  ( watch
  , server
  ) where

--------------------------------------------------------------------------------
import Control.Concurrent

--------------------------------------------------------------------------------
import Logger (Logger)
import qualified Logger as Logger

--------------------------------------------------------------------------------
import Watcher (watchUpdates)

import Server

-- | Watch for changes
watch :: FilePath -> Logger -> String -> Int -> Bool -> IO ()
watch targetDir logger host port runServer = do
  _ <- forkIO $ watchUpdates targetDir logMessage
  server'
  where
    loop = threadDelay 100000 >> loop
    server' =
      if runServer
        then server targetDir logger host port
        else loop
    logMessage = Logger.header logger "detected changes, reloading ..."

--------------------------------------------------------------------------------
-- | Start a server
server :: FilePath -> Logger -> String -> Int -> IO ()
server targetDir logger host port = staticServer logger targetDir host port
