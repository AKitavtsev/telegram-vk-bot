{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servises.Config
  ( Handle(..)
  , ForModule(..)
  , Config(..)
  , getLogConfig
  , getBotConfig
  ) where

import Servises.Data (Priority(..))

data ForModule
  = LOG
  | BOT
  deriving (Eq, Ord, Show)

newtype Handle =
  Handle
    { getConfig :: ForModule -> IO Config
    }

data Config
  =  LogConfig
      { level :: !Priority
      }
  |  BotConfig
      { сonfigApi :: !String
      , group_id :: !String
      , сonfigToken :: !String
      , сonfigNumberRepeat :: !Int
      , messageForRepeat :: !String
      , messageForHelp :: !String
      , myTimeout :: !Int
      }
  deriving (Show)

getLogConfig, getBotConfig :: Handle -> IO Config

getLogConfig = (`getConfig` LOG)

getBotConfig = (`getConfig` BOT)
