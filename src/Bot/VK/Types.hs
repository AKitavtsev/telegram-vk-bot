{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Types where

import Data.Aeson
import Data.Maybe (fromMaybe, isJust)
import GHC.Generics
import Text.Read (readMaybe)

import qualified Data.Text as T

import Bot
import JsonDrop

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

instance Upd Event where
  usId e = m_from_id $ m_message $ e_object e
  mesId e = m_id $ m_message $ e_object e
  txt e = m_text $ m_message $ e_object e
  getUserAndNumRep = map fgets
    where
      fgets x = (m_from_id $ m_message $ e_object x, payload x)
      payload x =
        fromMaybe
          0
          (readMaybe $ fromMaybe "0" $ m_payload $ m_message $ e_object x :: Maybe Int)
  listUpdWithKey = filter (\x -> isJust (m_payload (m_message $ e_object x)))

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
    , m_id :: Int
    , m_text :: T.Text
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
