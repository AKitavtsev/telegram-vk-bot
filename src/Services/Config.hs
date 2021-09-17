module Services.Config
  ( Handle(..)
  , Config(..)
  ) where

import Services.Types

newtype Handle =
  Handle
    { getConfig :: IO Config
    }

data Config =
  Config
    { levelLog :: !Priority
    , сonfigApi :: !Api
    , groupId :: !String
    , сonfigToken :: !String
    , сonfigNumberRepeat :: !Int
    , messageForRepeat :: !String
    , messageForHelp :: !String
    , myTimeout :: !Int
    }
  deriving (Show)
