module Services.Logger
  ( Handle(..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  ) where

import Services.Types (Priority(..))

newtype Handle =
  Handle
    { logPriority :: Priority -> String -> IO ()
    }

logDebug, logInfo, logWarning, logError :: Handle -> String -> IO ()
logDebug = (`logPriority` DEBUG)

logInfo = (`logPriority` INFO)

logWarning = (`logPriority` WARN)

logError = (`logPriority` ERROR)
