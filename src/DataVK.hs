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
    
