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
