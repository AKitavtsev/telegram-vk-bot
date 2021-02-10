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

appVK :: BC.ByteString
appVK = "api.vk.com"