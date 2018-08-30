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

watchUpdates :: FilePath -> IO () -> IO ()
watchUpdates providerDir action = do
  shouldBuild <- newEmptyMVar
  fullProviderDir <- canonicalizePath $ providerDir
  manager <- FSNotify.startManager
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
  void $
    FSNotify.watchTree manager providerDir (not . isRemove) $ \event -> do
      when True $ void $ tryPutMVar shouldBuild event
  where
    update' _ _ = action

isRemove :: FSNotify.Event -> Bool
isRemove (FSNotify.Removed {}) = True
isRemove _ = False
