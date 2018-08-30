-- Copyright (c) 2009 - 2017, Jasper Van der Jeugt
-- 
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Jasper Van der Jeugt nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
-- | Produce pretty, thread-safe logs
module Logger
  ( Verbosity(..)
  , Logger
  , new
  , flush
  , error
  , header
  , message
  , debug
  ) where

--------------------------------------------------------------------------------
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO, liftIO)
import Prelude hiding (error)

--------------------------------------------------------------------------------
data Verbosity
  = Error
  | Message
  | Debug
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Logger structure. Very complicated.
data Logger = Logger
  { loggerChan :: Chan (Maybe String) -- ^ Nothing marks the end
  , loggerSync :: MVar () -- ^ Used for sync on quit
  , loggerSink :: String -> IO () -- ^ Out sink
  , loggerVerbosity :: Verbosity -- ^ Verbosity
  }

--------------------------------------------------------------------------------
-- | Create a new logger
new :: Verbosity -> IO Logger
new vbty = do
  logger <- Logger <$> newChan <*> newEmptyMVar <*> pure putStrLn <*> pure vbty
  _ <- forkIO $ loggerThread logger
  return logger
  where
    loggerThread logger =
      forever $ do
        msg <- readChan $ loggerChan logger
        case msg
            -- Stop: sync
              of
          Nothing -> putMVar (loggerSync logger) ()
            -- Print and continue
          Just m -> loggerSink logger m

--------------------------------------------------------------------------------
-- | Flush the logger (blocks until flushed)
flush :: Logger -> IO ()
flush logger = do
  writeChan (loggerChan logger) Nothing
  () <- takeMVar $ loggerSync logger
  return ()

--------------------------------------------------------------------------------
string ::
     MonadIO m
  => Logger -- ^ Logger
  -> Verbosity -- ^ Verbosity of the string
  -> String -- ^ Section name
  -> m () -- ^ No result
string l v m
  | loggerVerbosity l >= v = liftIO $ writeChan (loggerChan l) (Just m)
  | otherwise = return ()

--------------------------------------------------------------------------------
error :: MonadIO m => Logger -> String -> m ()
error l m = string l Error $ "  [ERROR] " ++ m

--------------------------------------------------------------------------------
header :: MonadIO m => Logger -> String -> m ()
header l = string l Message

--------------------------------------------------------------------------------
message :: MonadIO m => Logger -> String -> m ()
message l m = string l Message $ "  " ++ m

--------------------------------------------------------------------------------
debug :: MonadIO m => Logger -> String -> m ()
debug l m = string l Debug $ "  [DEBUG] " ++ m
