{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE FlexibleContexts  #-}

module DataVK
  where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import           Data.List
import           Data.Text                    (Text)
import           GHC.Generics

import qualified Data.ByteString.Char8 as BC

import Drop

appVK :: BC.ByteString
appVK = "api.vk.com"

data Vk_Response a = Vk_Response
  {
    response     :: a
  } deriving (Show, Generic, FromJSON)
   
type SessionResponse = Vk_Response Session

data Session = Session
    { key :: String
    , server :: String
    , ts :: String
    } deriving (FromJSON, Show, Generic)
    
data Answer = Answer
    { a_ts :: String
    , a_updates :: [Event] 
    } deriving (Show, Generic)
    
instance FromJSON Answer where
  parseJSON = parseJsonDrop 2 
  
data Event = Event 
    { e_type :: String
    , e_object :: Vk_Message
    , e_group_id :: Integer
    } deriving (Show, Generic)
    
instance FromJSON Event where
  parseJSON = parseJsonDrop 2
 
data Vk_Message = Vk_Message 
    { m_message :: Vk_ItemMessage
    } deriving (Show, Generic)
    
instance FromJSON Vk_Message where
  parseJSON = parseJsonDrop 2
    
data Vk_ItemMessage = Vk_ItemMessage 
    { m_from_id :: Integer
    , m_id :: Integer
    , m_text :: String
    } deriving (Show, Generic)
    
instance FromJSON Vk_ItemMessage where
  parseJSON = parseJsonDrop 2
    
