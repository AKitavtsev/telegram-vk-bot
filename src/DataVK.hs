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

import Data

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
    { m_id :: Integer
    , m_user_id :: Integer
    , m_from_id :: Maybe Integer
    , m_title :: String
    , m_body :: String
    , m_date :: Integer
    , m_read_state :: Int
    -- , m_attachments :: []
    , m_emoji :: Maybe Int
    , m_important :: Maybe Int
    , m_deleted :: Maybe Int
    } deriving (Show, Generic)
    
instance FromJSON Vk_Message where
  parseJSON = parseJsonDrop 2
    
