--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Watcher
  ( watchUpdates
  ) where

--------------------------------------------------------------------------------
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (AsyncException, fromException, handle, throw)
import Control.Monad (forever, void, when)
import System.Directory (canonicalizePath)
import qualified System.FSNotify as FSNotify
import System.FilePath (pathSeparators)
#ifdef mingw32_HOST_OS
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, throw, try)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(ReadMode), hClose, openFile)
import System.IO.Error (isPermissionError)
#endif
--------------------------------------------------------------------------------
-- | A thread that watches for updates in a 'providerDirectory' and recompiles
-- a site as soon as any changes occur
watchUpdates :: FilePath -> IO () -> IO ()
watchUpdates providerDir action = do
  shouldBuild <- newEmptyMVar
  fullProviderDir <- canonicalizePath $ providerDir
  manager <- FSNotify.startManager
    -- This thread continually watches the `shouldBuild` MVar and builds
    -- whenever a value is present.
  _ <-
    forkIO $
    forever $ do
      event <- takeMVar shouldBuild
      handle
        (\e ->
           case fromException e of
             Nothing -> putStrLn (show e)
             Just async -> throw (async :: AsyncException))
        (update' event providerDir)
    -- Send an event whenever something occurs so that the thread described
    -- above will do a build.
  void $
    FSNotify.watchTree manager providerDir (not . isRemove) $ \event -> do
      when True $ void $ tryPutMVar shouldBuild event
  where

#ifndef mingw32_HOST_OS
    update' _ _ = action
#else
    update' event provider = do
      let path = provider </> FSNotify.eventPath event
        -- on windows, a 'Modified' event is also sent on file deletion
      fileExists <- doesFileExist path
      when fileExists . void $ waitOpen path ReadMode (\_ -> action) 10
    
    -- continuously attempts to open the file in between sleep intervals
    -- handler is run only once it is able to open the file
    waitOpen :: FilePath -> IOMode -> (Handle -> IO r) -> Integer -> IO r
    waitOpen _ _ _ 0 = do
      putStrLn "[ERROR] Failed to retrieve modified file for regeneration"
      exitFailure
    waitOpen path mode handler retries = do
      res <- try $ openFile path mode :: IO (Either IOException Handle)
      case res of
        Left ex ->
          if isPermissionError ex
            then do
              threadDelay 100000
              waitOpen path mode handler (retries - 1)
            else throw ex
        Right h -> do
          handled <- handler h
          hClose h
          return handled
#endif
--------------------------------------------------------------------------------
isRemove :: FSNotify.Event -> Bool
isRemove (FSNotify.Removed {}) = True
isRemove _ = False
