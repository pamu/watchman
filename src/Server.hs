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

import Logger (log)
import Prelude hiding (log)

--------------------------------------------------------------------------------
staticServer :: FilePath -> String -> Int -> IO ()
staticServer directory host port = do
  log $ "Listening on http://" ++ host ++ ":" ++ show port
  Warp.runSettings warpSettings $
    Static.staticApp (Static.defaultFileServerSettings directory)
  where
    warpSettings =
      Warp.setLogger noLog $
      Warp.setHost (fromString host) $ Warp.setPort port Warp.defaultSettings

noLog :: Wai.Request -> Status -> Maybe Integer -> IO ()
noLog _ _ _ = return ()
