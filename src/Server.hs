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
-- | Implements a basic static file server for previewing options
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( staticServer
  ) where

--------------------------------------------------------------------------------
import Data.String
import Network.HTTP.Types.Status (Status)
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp

--------------------------------------------------------------------------------
import Logger (Logger)
import qualified Logger as Logger

staticServer ::
     Logger -- ^ Logger
  -> FilePath -- ^ Directory to serve
  -> String -- ^ Host to bind on
  -> Int -- ^ Port to listen on
  -> IO () -- ^ Blocks forever
staticServer logger directory host port = do
  Logger.header logger $ "Listening on http://" ++ host ++ ":" ++ show port
  Warp.runSettings warpSettings $
    Static.staticApp (Static.defaultFileServerSettings directory)
  where
    warpSettings =
      Warp.setLogger noLog $
      Warp.setHost (fromString host) $ Warp.setPort port Warp.defaultSettings

noLog :: Wai.Request -> Status -> Maybe Integer -> IO ()
noLog _ _ _ = return ()
