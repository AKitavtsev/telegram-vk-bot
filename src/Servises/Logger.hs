module Servises.Logger
  ( Handle(..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  ) where

import Servises.Config (Config(..))
import Servises.Data (Priority(..))

data Handle =
  Handle
    { config :: Config
    , logPriority :: Priority -> String -> IO ()
    }

logDebug, logInfo, logWarning, logError :: Handle -> String -> IO ()
logDebug = (`logPriority` DEBUG)

logInfo = (`logPriority` INFO)

logWarning = (`logPriority` WARN)

logError = (`logPriority` ERROR)
