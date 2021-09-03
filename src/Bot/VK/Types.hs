{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types where

import Data.Aeson
import GHC.Generics

import JsonDrop
import Session

appVK :: String
appVK = "https://api.vk.com/method/messages.send"

newtype VKResponse a =
  VKResponse
    { response :: a
    }
  deriving (Show, Generic, FromJSON)

type SessionResponse = VKResponse Session

data Answer =
  Answer
    { a_ts :: Maybe String
    , a_updates :: Maybe [Event]
    , a_failed :: Maybe Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON Answer where
  parseJSON = parseJsonDrop 2

data Event =
  Event
    { e_type :: String
    , e_object :: VKMessage
    , e_group_id :: Integer
    }
  deriving (Show, Eq, Generic)

instance FromJSON Event where
  parseJSON = parseJsonDrop 2

newtype VKMessage =
  VKMessage
    { m_message :: VKItemMessage
    }
  deriving (Show, Eq, Generic)

instance FromJSON VKMessage where
  parseJSON = parseJsonDrop 2

data VKItemMessage =
  VKItemMessage
    { m_from_id :: Int
    , m_random_id :: Integer
    , m_id :: Integer
    , m_text :: String
    , m_payload :: Maybe String
    }
  deriving (Show, Eq, Generic)

instance FromJSON VKItemMessage where
  parseJSON = parseJsonDrop 2

data Keyboard =
  Keyboard
    { buttons :: [[Button]]
    , inline :: Bool
    , one_time :: Bool
    }
  deriving (FromJSON, ToJSON, Show, Generic)

data Button =
  Button
    { action :: Action
    , color :: String
    }
  deriving (FromJSON, ToJSON, Show, Generic)

data Action =
  Action
    { a_type :: String
    , a_label :: String
    , a_payload :: String
    }
  deriving (Show, Generic)

instance ToJSON Action where
  toJSON = toJsonDrop 2

instance FromJSON Action where
  parseJSON = parseJsonDrop 2