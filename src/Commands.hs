-- Code taken from Hakyll project
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Commands(watch, server) where


--------------------------------------------------------------------------------
import           Control.Concurrent

--------------------------------------------------------------------------------
import           Logger         (Logger)
import qualified Logger as Logger

--------------------------------------------------------------------------------
import           Watcher        (watchUpdates)

import           Server

#ifdef mingw32_HOST_OS
import           Control.Monad              (void)
import           System.IO.Error            (catchIOError)
#endif


--------------------------------------------------------------------------------
-- | Watch for changes

watch :: FilePath -> Logger -> String -> Int -> Bool -> IO ()
watch targetDir logger host port runServer = do
#ifndef mingw32_HOST_OS
   _ <- forkIO $ watchUpdates targetDir logMessage
#else
   -- Force windows users to compile with -threaded flag, as otherwise
   -- thread is blocked indefinitely.
   catchIOError (void $ forkOS $ watchUpdates targetDir logMessage) $ do
       fail $ "Commands.watch: Could not start update watching " ++
              "thread. Did you compile with -threaded flag?"
#endif
   server'
 where
   loop = threadDelay 100000 >> loop
   server' = if runServer then server targetDir logger host port else loop
   logMessage = Logger.header logger "detected changes, reloading ..."


--------------------------------------------------------------------------------
-- | Start a server
server :: FilePath -> Logger -> String -> Int -> IO ()
server targetDir logger host port = staticServer logger targetDir host port
