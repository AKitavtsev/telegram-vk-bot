module Services.Config
  ( Handle(..)
  , Config (..)
  ) where

import Services.Types (Priority(..))

newtype Handle =
  Handle
    { getConfig :: IO Config
    }

data Config =
  Config
    { level :: !Priority
    , сonfigApi :: !String
    , groupId :: !String
    , сonfigToken :: !String
    , сonfigNumberRepeat :: !Int
    , messageForRepeat :: !String
    , messageForHelp :: !String
    , myTimeout :: !Int
    }
  deriving (Show)
