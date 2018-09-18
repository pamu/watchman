module Logger
  ( log
  ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Prelude hiding (log)

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn
