--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Commands
  ( watch
  , server
  ) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Logger (log)
import Prelude hiding (log)
import Server (staticServer)
import Watcher (watchUpdates)

--------------------------------------------------------------------------------
-- | Watch
watch :: FilePath -> String -> Int -> Bool -> IO ()
watch targetDir host port runServer = do
  _ <- forkIO $ watchUpdates targetDir logAction
  server'
  where
    loop = threadDelay 100000 >> loop
    server' =
      if runServer
        then server targetDir host port
        else loop
    logAction = log "detected changes, reloading ..."

--------------------------------------------------------------------------------
-- | Start a server
server :: FilePath -> String -> Int -> IO ()
server = staticServer
