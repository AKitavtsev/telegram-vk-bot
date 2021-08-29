{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Bot.VK.Data where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import           Data.List
-- import           Data.Text                    (Text)
-- import           GHC.Generics

import qualified Data.ByteString.Char8 as BC

import Drop

appVK :: String
appVK = "https://api.vk.com/method/messages.send"

data Vk_Response a = Vk_Response
  {
    response     :: a
  } deriving (Show, Generic, FromJSON)
   
type SessionResponse = Vk_Response Session

data Answer = Answer
    { a_ts :: Maybe String
    , a_updates :: Maybe [Event] 
    , a_failed :: Maybe Int
    } deriving (Show, Eq, Generic)
    
instance FromJSON Answer where
  parseJSON = parseJsonDrop 2 
  
data Event = Event 
    { e_type :: String
    , e_object :: Vk_Message
    , e_group_id :: Integer
    } deriving (Show, Eq, Generic)
    
instance FromJSON Event where
  parseJSON = parseJsonDrop 2
 
data Vk_Message = Vk_Message 
    { m_message :: Vk_ItemMessage
    } deriving (Show, Eq, Generic)
    
instance FromJSON Vk_Message where
  parseJSON = parseJsonDrop 2
    
data Vk_ItemMessage = Vk_ItemMessage 
    { m_from_id :: Int
    , m_random_id :: Integer
    , m_id :: Integer
    , m_text :: String
    , m_payload :: Maybe String
    } deriving (Show, Eq, Generic)
    
instance FromJSON Vk_ItemMessage where
  parseJSON = parseJsonDrop 2
  
data Keyboard = Keyboard
  { buttons :: [[Button]]
  , inline :: Bool
  , one_time :: Bool
  } deriving (FromJSON, ToJSON, Show, Generic)
  
data Button = Button
  { action :: Action
  , color :: String
  } deriving (FromJSON, ToJSON, Show, Generic)
  
data Action = Action
  { a_type :: String
  , a_label :: String
  , a_payload :: String
  } deriving (Show, Generic)
  
instance ToJSON Action where
  toJSON = toJsonDrop 2
instance FromJSON Action where
  parseJSON = parseJsonDrop 2
    



    
